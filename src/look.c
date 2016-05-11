/*-
 * Copyright (c) 1991, 1993
 *  The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * David Hitz of Auspex Systems, Inc.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *  This product includes software developed by the University of
 *  California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */

#include <Rinternals.h>
#include <string.h>
#include <stdlib.h>

#include <sys/types.h>
#include <sys/mman.h>
#include <sys/stat.h>

#include <fcntl.h>
#include <limits.h>
#include <locale.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <wchar.h>
#include <wctype.h>
#include <errno.h>

#define EQUAL   0
#define GREATER   1
#define LESS    (-1)

int dflag, fflag;

unsigned char    *binary_search(wchar_t *, unsigned char *, unsigned char *);
int              compare(wchar_t *, unsigned char *, unsigned char *);
unsigned char    *linear_search(wchar_t *, unsigned char *, unsigned char *);
wchar_t          *prepkey(const char *, wchar_t);

SEXP call_look(SEXP filename, SEXP term, SEXP termEnd, SEXP nSkip, SEXP sMaxLines,
                SEXP sDflag)  {

  SEXP output = PROTECT(allocVector(RAWSXP, 0));

  dflag = INTEGER(sDflag)[0];
  fflag = 0;

  struct stat sb;
  int j;
  int nlines = 0;
  int n = INTEGER(nSkip)[0];
  int maxLines = INTEGER(sMaxLines)[0];
  int ch, fd, match;
  wchar_t termchar;
  unsigned char *back, *front, *true_back, *head;
  const char *file;
  wchar_t *key;
  wchar_t *keyEnd;

  (void) setlocale(LC_CTYPE, "");

  termchar = L'\0';
  file = CHAR(STRING_ELT(filename, 0));

  match = 1;
  key = prepkey(CHAR(STRING_ELT(term, 0)),termchar);
  keyEnd = prepkey(CHAR(STRING_ELT(termEnd, 0)),termchar);

  if ((fd = open(file, O_RDONLY, 0)) < 0 || fstat(fd, &sb)) {
    UNPROTECT(1);
    return output;
  }
  if (sb.st_size == 0) {
    close(fd);
    UNPROTECT(1);
    return output;
  }
  if ((head = front = mmap(NULL, (size_t)sb.st_size, PROT_READ, MAP_SHARED, fd, (off_t)0)) == MAP_FAILED) {
    UNPROTECT(1);
    Rf_error("Failed to mmap %s: %s", file, strerror(errno));
    return output;
  }

  back = front + sb.st_size;

  /* Skip the first two header rows */

  for (j = 0; j < n; j++) {
    front = memchr(front,'\n',back - front) + 1;
    if (front == NULL) {
      UNPROTECT(1);
      munmap(head, back - head);
      return output;
    }
  }

  front = binary_search(key, front, back);
  front = linear_search(key, front, back);

  close(fd);

  if (front == NULL) {
    munmap(head, back - head);
    UNPROTECT(1);
    return output;
  }

  true_back = front;

  for (; true_back < back && compare(keyEnd, true_back, back) != LESS; ++true_back) {
    for (; true_back < back && *true_back != '\n'; ++true_back) {}
    nlines++;
    if (nlines == maxLines) break;
  }

  if (true_back <= front) {
    UNPROTECT(1);
    munmap(head, back - head);
    return output;
  }

  output = PROTECT(allocVector(RAWSXP, true_back - front));
  char * output_p = (char *) RAW(output);
  memcpy(output_p, front, true_back - front);

  UNPROTECT(2);
  munmap(head, back - head);
  return output;
}

wchar_t *
prepkey(const char *string, wchar_t termchar)
{
  const char *readp;
  wchar_t *key, *writep;
  wchar_t ch;
  size_t clen;

  /*
   * Reformat search string and convert to wide character representation
   * to avoid doing it multiple times later.
   */
  if ((key = malloc(sizeof(wchar_t) * (strlen(string) + 1))) == NULL)
    return(NULL);
  readp = string;
  writep = key;
  while ((clen = mbrtowc(&ch, readp, MB_LEN_MAX, NULL)) != 0) {
    if (clen == (size_t)-1 || clen == (size_t)-2)
      return(NULL);
    if (fflag)
      ch = towlower(ch);
    if (!dflag || iswalnum(ch))
      *writep++ = ch;
    readp += clen;
  }
  *writep = L'\0';
  if (termchar != L'\0' && (writep = wcschr(key, termchar)) != NULL)
    *++writep = L'\0';
  return (key);
}

/*
 * Binary search for "string" in memory between "front" and "back".
 *
 * This routine is expected to return a pointer to the start of a line at
 * *or before* the first word matching "string".  Relaxing the constraint
 * this way simplifies the algorithm.
 *
 * Invariants:
 *  front points to the beginning of a line at or before the first
 *  matching string.
 *
 *  back points to the beginning of a line at or after the first
 *  matching line.
 *
 * Base of the Invariants.
 *  front = NULL;
 *  back = EOF;
 *
 * Advancing the Invariants:
 *
 *  p = first newline after halfway point from front to back.
 *
 *  If the string at "p" is not greater than the string to match,
 *  p is the new front.  Otherwise it is the new back.
 *
 * Termination:
 *
 *  The definition of the routine allows it return at any point,
 *  since front is always at or before the line to print.
 *
 *  In fact, it returns when the chosen "p" equals "back".  This
 *  implies that there exists a string is least half as long as
 *  (back - front), which in turn implies that a linear search will
 *  be no more expensive than the cost of simply printing a string or two.
 *
 *  Trying to continue with binary search at this point would be
 *  more trouble than it's worth.
 */
#define SKIP_PAST_NEWLINE(p, back) \
  p = memchr(p, '\n', back - p);

unsigned char *
binary_search(wchar_t *string, unsigned char *front, unsigned char *back)
{
  unsigned char *p;

  p = front + (back - front) / 2;
  SKIP_PAST_NEWLINE(p, back);
  p++;

  /*
   * If the file changes underneath us, make sure we don't
   * infinitely loop.
   */
  while (p < back && back > front) {
    if (compare(string, p, back) == GREATER)
      front = p;
    else
      back = p;
    p = front + (back - front) / 2;
    SKIP_PAST_NEWLINE(p, back);
    p++;
  }
  return (front);
}

/*
 * Find the first line that starts with string, linearly searching from front
 * to back.
 *
 * Return NULL for no such line.
 *
 * This routine assumes:
 *
 *  o front points at the first character in a line.
 *  o front is before or at the first line to be printed.
 */
unsigned char *
linear_search(wchar_t *string, unsigned char *front, unsigned char *back)
{
  while (front < back) {
    switch (compare(string, front, back)) {
    case EQUAL:   /* Found it. */
      return (front);
    case LESS:    /* No such string. */
      return (NULL);
    case GREATER:   /* Keep going. */
      break;
    }
    SKIP_PAST_NEWLINE(front, back);
    front++;
  }
  return (NULL);
}


/*
 * Return LESS, GREATER, or EQUAL depending on how the string1 compares with
 * string2 (s1 ??? s2).
 *
 *  o Matches up to len(s1) are EQUAL.
 *  o Matches up to len(s2) are GREATER.
 *
 * Compare understands about the -f and -d flags, and treats comparisons
 * appropriately.
 *
 * The string "s1" is null terminated.  The string s2 is '\n' terminated (or
 * "back" terminated).
 */
int
compare(wchar_t *s1, unsigned char *s2, unsigned char *back)
{
  wchar_t ch1, ch2;
  size_t len2;

  for (; *s1 && s2 < back && *s2 != '\n'; ++s1, s2 += len2) {
    ch1 = *s1;
    len2 = mbrtowc(&ch2, (char *) s2, back - s2, NULL);
    if (len2 == (size_t)-1 || len2 == (size_t)-2) {
      ch2 = *s2;
      len2 = 1;
    }
    if (fflag)
      ch2 = towlower(ch2);
    if (dflag && !iswalnum(ch2)) {
      /* Ignore character in comparison. */
      --s1;
      continue;
    }
    if (ch1 != ch2)
      return (ch1 < ch2 ? LESS : GREATER);
  }
  return (*s1 ? GREATER : EQUAL);
}


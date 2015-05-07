#' Display lines of a file
#'
#' This function provides a more direct analogue to the
#' BSD function 'look' for users looking for more control
#' on the output. The file must be pre-sorted on the row
#' prefixes
#'
#' @importFrom       iotools dstrsplit
#' @useDynLib        fileDB call_look
#' @param file       name of the file to search within
#' @param key        a length one character vector; the key
#'                   to search for.
#' @param keyEnd     an optional length one character vector;
#'                   if set, results will be given for all rows
#'                   with starting characters between key and keyEnd
#' @param dflag      logical; flag for whether only alphanumeric
#'                   characters are being searched over. When set,
#'                   the function will run slightly more efficiently.
#' @param skip       number of rows to skip in the input file; these do
#'                   not need to be sorted (i.e., a row of headers or
#'                   rows of comments)
#' @param maxLines   maximum number of lines to return. Set to Inf
#'                   or -1 to return all lines.
#' @param raw        logical. Should results be returned as a raw
#'                   vector, or a character vector split by newline
#'                   characters.
#' @export
look = function(file, key, keyEnd=NULL, dflag=FALSE, skip=0L,
                maxLines = -1L, raw=FALSE) {
  file = Sys.glob(file[[1]])
  key = key[[1]]

  if (!file.exists(file <- as.character(file)))
    stop("file does not point to a valid file")
  if (length(key <- as.character(key)) != 1) {
    if (length(key) == 0) stop("invalid key")
    key = key[[1]]
    warning("Key truncated; only using first key")
  }
  if (is.null(keyEnd))
    keyEnd = key
  else if (length(keyEnd <- as.character(keyEnd)) != 1) {
    if (length(keyEnd) == 0) stop("invalid key")
    keyEnd = keyEnd[[1]]
    warning("keyEnd truncated; only using first keyEnd")
  }
  if (!is.finite(maxLines <- maxLines[[1]])) maxLines = -1L
  maxLines = as.integer(maxLines[[1]])

  if (!(dflag <- as.integer(dflag[1]) %in% c(0L,1L)))
    stop("Invalid dflag")

  z = .Call(call_look, file, key, keyEnd, skip, maxLines, dflag)
  if (raw) return(z) else unlist(strsplit(rawToChar(z),"\n"))
}

#' Search for key in fileDB file
#'
#' This function provides a means for searching a sorted
#' flat file constructed by \code{\link{saveAsFileDB}}.
#' Use \code{link{scanFile}} for a range query.
#'
#' @importFrom       iotools dstrsplit
#' @useDynLib        fileDB call_look
#' @param file       name of the file to search within
#' @param key        a length one character vector; the key
#'                   to search for.
#' @param formatter  function which accepts a raw vector and
#'                   returns the results. If missing, this will
#'                   by constructed via the file metadata.
#' @param maxLines   maximum number of lines to return. Set to Inf
#'                   or -1 to return all lines.
#' @param header     logical. Whether file has a head of information,
#'                   as supplied by the function saveAsFileDB. When
#'                   missing, will be infered from the first line of
#'                   the file.
#' @export
searchFile = function(file, key, formatter, maxLines=100L, header) {
  file = Sys.glob(file[[1]])
  key = key[[1]]

  if (!file.exists(file <- as.character(file)))
    stop("file does not point to a valid file")
  if (length(key <- as.character(key)) != 1) {
    if (length(key) == 0) stop("invalid key")
    key = key[[1]]
    warning("Key truncated; only using first key")
  }
  if (!is.finite(maxLines <- maxLines[[1]])) maxLines = -1L
  maxLines = as.integer(maxLines[[1]])

  # Header / Metadata
  h = readLines(file,2L)
  if (missing(header)) {
    header = (substr(h[1],1L,3L) == "###")
  } else header = as.logical(header[[1]])

  if (missing(formatter)) {
    if (!header)
      formatter = function(v)
        iotools::mstrsplit(unlist(strsplit(rawToChar(v),"\n")))
    else {
      h[1] = substr(h[1],4L, nchar(h[1]))
      h = strsplit(h,"\\|")
      names(h[[1]]) = h[[2]]
      formatter = function(v) iotools::dstrsplit(v, col_types=h[[1]])
    }
  }

  z = .Call(call_look, file, key, key, 2L*header, maxLines, 0L)
  formatter(z)
}

#' Scan for key in fileDB file
#'
#' This function provides a means for scan a sorted
#' flat file constructed by \code{\link{saveAsFileDB}},
#' returning all rows with prefixes between two keys.
#'
#' @importFrom       iotools dstrsplit mstrsplit
#' @useDynLib        fileDB call_look
#' @param file       name of the file to search within
#' @param keyStart   a length one character vector; the key
#'                   to start searching for.
#' @param keyEnd     a length one character vector; the key
#'                   to start searching for. Results will be
#'                   given for all rows with starting characters
#'                   between keyStart and keyEnd
#' @param formatter  function which accepts a raw vector and
#'                   returns the results. If missing, this will
#'                   by constructed via the file metadata.
#' @param maxLines   maximum number of lines to return. Set to Inf
#'                   or -1 to return all lines.
#' @param header     logical. Whether file has a head of information,
#'                   as supplied by the function saveAsFileDB. When
#'                   missing, will be infered from the first line of
#'                   the file.
#' @export
scanFile = function(file, keyStart, keyEnd, formatter,
                    maxLines=100L, header) {
  file = Sys.glob(file[[1]])
  keyStart = keyStart[[1]]

  if (!file.exists(file <- as.character(file)))
    stop("file does not point to a valid file")
  if (length(keyStart <- as.character(keyStart)) != 1) {
    if (length(keyStart) == 0) stop("invalid keyStart")
    keyStart = keyStart[[1]]
    warning("Key truncated; only using first keyStart")
  }
  if (length(keyEnd <- as.character(keyEnd)) != 1) {
    if (length(keyEnd) == 0) stop("invalid keyEnd")
    keyEnd = keyEnd[[1]]
    warning("Key truncated; only using first keyEnd")
  }
  if (!is.finite(maxLines <- maxLines[[1]])) maxLines = -1L
  maxLines = as.integer(maxLines[[1]])

  # Header / Metadata
  h = readLines(file,2L)
  if (missing(header)) {
    header = (substr(h[1],1L,3L) == "###")
  } else header = as.logical(header[[1]])

  if (missing(formatter)) {
    if (!header)
      formatter = function(v)
        iotools::mstrsplit(unlist(strsplit(rawToChar(v),"\n")))
    else {
      h[1] = substr(h[1],4L, nchar(h[1]))
      h = strsplit(h,"\\|")
      names(h[[1]]) = h[[2]]
      formatter = function(v) iotools::dstrsplit(v, col_types=h[[1]])
    }
  }

  z = .Call(call_look, file, keyStart, keyEnd, 2L*header, maxLines, 0L)
  formatter(z)
}

#' Save a data frame as fileDB file
#'
#' This function saves a data frame as a flat file
#' that can be searched using \code{\link{scan}}
#' and \code{\link{search}}. The first row of the
#' output is a commented out string of column types;
#' the second row are column names and the remainder
#' is a pipe-deliminated file. Conveniently, this can
#' also be read back in using \code{\link{read.table}},
#' by setting sep='|'.
#'
#' @importFrom       iotools as.output.data.frame
#' @param df         data frame to write to disk. First column
#'                   will be 'indexed'; must be a character or
#'                   factor as numerics will not sort correctly
#'                   unless they all have the same number of digits
#' @param file       name of the file to store the results in.
#' @param header     logical. Should two header rows be appended to
#'                   the output.
#' @export
saveAsFileDB = function(df, file, header=TRUE) {
  if (!is.data.frame(df))
    stop("df must be a data frame")

  df = df[order(df[,1]),]
  r = iotools::as.output.data.frame(df,keys=FALSE)
  con = file(description=file, open="wb")
  on.exit(close(con))

  classes = sapply(df, function(v) class(v[[1]]))
  classes[classes == "factor"] = "character"
  classTypes = c("logical", "integer", "numeric", "complex",
                 "character", "raw", "POSIXct")

  if (!all(classes %in% classTypes))
    stop("Cannot save a dataframe with these types.")
  if (classes[1] != "character")
    stop("First column, the one which is sorted on",
         "must be of type 'character' or 'factor'")

  if (header) {
    writeBin(charToRaw("###"), con)
    writeBin(charToRaw(paste(classes,collapse="|")), con)
    writeBin(charToRaw("\n"), con)

    writeBin(charToRaw(paste(names(df),collapse="|")), con)
    writeBin(charToRaw("\n"), con)
  }

  writeBin(r, con)
}


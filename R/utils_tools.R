#' @title ifelseProper
#'
#' @description
#'  \code{link[base]{basename}} replacement for properly returning all
#'  datatypes. Note: unlike the regular function, this one is NOT vectorized.
#'  Also: no optimization, so there will be a performance 'penalty'
#'
#' @param logicValue variable or expression resulting in TRUE or FALSE,
#'  if missing or not logical then the function will return NULL.
#' @param ifTrue variable or expression to be returned when logicValue == TRUE
#' @param ifFalse variable or expression to be returned when logicValue == FALSE
#'
#' @returns depending on logicValue, ifTrue or ifFalse
#' @export
#'
#' @examples
#' ifelse(mtcars$cyl == 6, "Six", "Other")
#' ifelse(mtcars$cyl == 6, list("Six", TRUE), list("Other", FALSE))
#' ifelse(mtcars$cyl == 6, list("Six", TRUE), list("Other", FALSE))[[1]]
#' ifelse(mtcars$cyl[1] == 6, list("Six", TRUE), list("Other", FALSE))
#' ifelseProper(mtcars$cyl[1] == 6, list("Six", TRUE), list("Other", FALSE))
#' purrr::map(mtcars$cyl, ~ifelseProper(.x == 6, list("Six", TRUE), list("Other", FALSE)))
#' purrr::map(mtcars$cyl, ~ifelseProper(.x == 6, list("Six", TRUE), list("Other", FALSE)))[[1]]
ifelseProper <- function(logicValue = NULL, ifTrue = NULL, ifFalse = NULL) {
  if (missing(logicValue)) {
    return(NULL)
  } else {
    if (!is.logical(logicValue)) {
      return(NULL)
    } else {
      if (logicValue) {
        return(ifTrue)
      } else {
        return(ifFalse)
      }
    }
  }
}

#' @title readFile
#'
#' @description
#'  function factory that generates a file reading function that can take into
#'  account that a reading file action may fail. It allows for use of different
#'  read functions to be used via functional programming. The result of the
#'  function will be NA if the file read action fails.
#'
#'
#' @param filename name of the file (including path) to be read. Note: filenames
#'  are automatically trimmed (removal of spaces).
#' @param readFunction specifies the function to used to read the file
#' @param stopOnError logical vector specifying if execution should stop on
#'  error. Default is FALSE
#' @param showWarning logical vector specifying whether to show warnings.
#'  Default is TRUE
#'
#' @returns a function for reading the file with name filename (incl path).
#'  This function will return NA if reading the file fails. Execution will not
#'  stop unless specified.
#'
#' @export
#'
#' @examples
#' filename <- tempfile()
#' write.csv(mtcars, filename)
#' readFile(filename = filename, readFunction = read.csv)()
#' write.table(mtcars, filename, col.names = TRUE, sep = ";")
#' readFile(filename = filename, readFunction = read.csv)(header = TRUE, sep = ";")
#' file.remove(filename)
#' readFile(filename = filename, readFunction = read.csv)(header = TRUE, sep = ";")
readFile <- function(filename, readFunction = XML::xmlToList,
                     stopOnError = FALSE, showWarning = TRUE) {
  force(filename)
  force(readFunction)
  force(stopOnError)
  function(...){
    if (!is.na(filename)){
      filename <- stringr::str_trim(filename, side = "both")
    }
    result <- NA
    result <- tryCatch(
      {
        if (!is.na(filename)){
          theFile <- readFunction(filename, ...)
        } else {
          theFile <- readFunction(...)
        }
      },
      warning = function(war) {
        if (showWarning){
          warning(war)
        }
        return(NA)
      },
      error = function(err) {
        if (stopOnError){
          stop(err)
        } else {
          # warning(err)
          return(NA)
        }
      }
    )
    return(result)
  }
}

#' @title formatDigitsLargeNumbers
#'
#' @description Function factory to be used to specify the number of digits to
#'  be used in large numbers. The function generates numbers as strings w/o big
#'  marks (US/UK commas)
#'
#' @param digits integer value that specifies the number of digits to be used
#'  by the resulting function
#'
#' @returns a function that will take a numeric vector as an argument and
#'  returns a character vector of the numeric vector with the set number of
#'  digits (see ?scales::lebel_number for more info) but w/o big marks
#'
#' @export
#'
#' @examples
#' formatDigitsLargeNumbers(4)(319.65897)
#' formatDigitsLargeNumbers(5)(-6.320)
formatDigitsLargeNumbers <- function(digits){
  function(v){
    return(scales::comma(v, accuracy = 10^(-digits), big.mark = ""))
  }
}

#' @title fieldsPresent
#'
#' @description function that checks if one or more elements with a defined name
#'  are present in a list. It also can check if the fields (if present) are NA
#'
#' @param theList list object with named elements
#' @param fields  character vector of the names which are to be tested for being
#'  present
#' @param allowNA if FALSE (default) then any element with a name in 'fields'
#'  present which is NA will be counted as not present. If TRUE, then this is
#'  ignored
#'
#' @returns a logical vector
#' @export
#'
#' @examples
#' testList <- list(
#'  mtcars = mtcars,
#'  iris = iris,
#'  test = NA
#' )
#' fieldsPresent(theList = testList, fields = c("mtcars", "iris"))
#' fieldsPresent(theList = testList, fields = c("mtcars", "airquality"))
#' fieldsPresent(theList = testList, fields = c("mtcars", "test"))
#' fieldsPresent(theList = testList, fields = c("mtcars", "test"), allowNA = TRUE)
fieldsPresent <- function(theList, fields, allowNA = FALSE){
  allPresent <- purrr::map_lgl(fields, ~.x %in% names(theList))
  allPresent <- sum(allPresent) == length(fields)
  if (allPresent & !allowNA){
    allPresent <- purrr::map_lgl(fields, ~!identical(theList[[.x]], NA))
  }
  return(sum(allPresent) == length(allPresent))
}

# examples positive: "_MS.csv$", ".CSV$", "mzdata.xml$
#

#' @title selectStrings
#'
#' @description
#'  select a subset from a character vector base on a substring that should be
#'  present (positive criterium) and/or a substring that should not be present
#'  (negative criterium). Originally meant to be able to filter filenames, but
#'  can be used on other character vectors
#'
#' @param strings character vector to be filtered
#' @param positive character vector to be used as a positive criterium pattern
#'  via \code{link[base]{grepl}}. To put it simple if the 'positive' character
#'  is present in the 'strings' character vector then it will be included, see
#'  examples
#' @param negative character vector to be used as a negative criterium pattern
#'  via \code{link[base]{grepl}}. To put it simple if the 'negative' character
#'  is present in the 'strings' character vector then it will not be included,
#'  see examples
#' @param returnStrings logical vector determines whether to return the actual
#'  strings (which comply with the positive and the negative criteria) (default)
#'  or the position/ See examples
#' @param useBasename logical vector. If TRUE, then the filtering takes place
#'  after \code{link[base]{basename}} has been appied on the 'strings' argument.
#'  Note that the returned strings (when returnStrings is TRUE) will be the
#'  original values.
#' @param stopOnError logical vector specifying if execution should stop on
#'  error. Default is FALSE
#' @param showWarning logical vector specifying whether to show warnings.
#'  Default is TRUE
#'
#' @returns character or integer vector
#' @export
#'
#' @examples
#' selectStrings(strings = c("test_MS.csv", "testMS.csv"),
#'               positive = "_MS.csv$")
#' selectStrings(strings = c("test_MS.csv", "testMS.csv"),
#'               positive = "MS.csv$")
#' selectStrings(strings = c("test_MS.csv", "testMS.csv"),
#'               positive = "MS.csv$", negative = "_")
#' selectStrings(strings = c("test_MS.csv", "testMS.csv"),
#'               positive = "MS.csv$", negative = "_",
#'               returnStrings = FALSE)
#' selectStrings(strings = c("test_MS.csv", "testMS.csv"),
#'               positive = "MS.csb$", negative = "_")
selectStrings <- function(strings,
                          positive = NA, negative = NA,
                          returnStrings = TRUE,
                          useBasename = TRUE,
                          stopOnError = FALSE, showWarning = TRUE){
  tempStrings <- strings
  result <- tryCatch(
    {
      if (!is.na(positive)){
        if (!useBasename){
          strings[grepl(strings, pattern = positive)]
        } else {
          strings <- strings[grepl(basename(strings), pattern = positive)]
        }
      }
      if (!is.na(negative)){
        if (!useBasename){
          strings <- strings[!grepl(strings, pattern = negative)]
        } else {
          strings <- strings[!grepl(basename(strings), pattern = negative)]
        }
      }
    },
    warning = function(war) {
      if (showWarning){
        warning(war)
      }
      return(NA)
    },
    error = function(err) {
      if (stopOnError){
        stop(err)
      } else {
        warning(err)
        return(NA)
      }
    }
  )
  if (identical(result, NA)){
    strings <- tempStrings
  }
  if (returnStrings){
    return(strings)
  } else {
    return(purrr::map_int(strings, ~which(.x == tempStrings)))
  }
}

#' @title giveFilePathsFromStrings
#'
#' @description
#'  Function that extracts the directory (path) part from a set of file
#'  locations. This function eliminates character vectors parts that contain the
#'  substring '.mzdata' (files with extension '.mzdata.xml') and names that have
#'  the pattern '_.*$'
#'
#' @param strings character vector, usually the result of
#'  \code{link[base]{list.files}}
#'
#' @returns character vector (can be character(0))
#' @export
#'
#' @examples
#' giveFilePathsFromStrings("R:/testfiles/test.mzdata.xml")
#' giveFilePathsFromStrings(c("R:/testfiles/test.xml", "R:/test2/test5.csv"))
#' giveFilePathsFromStrings(c("R:/testfiles/test.xml", "R:/test2/test5.csv",
#'                            "R:/testfiles/test.mzdata.xml"))
#' giveFilePathsFromStrings(c("R:/testfiles/test.xml", "R:/test2/test5.csv",
#'                            "R:/testfiles/test6.mzdata.xml"))
giveFilePathsFromStrings <- function(strings){
  result <- tools::file_path_sans_ext(strings)
  result <- stringr::str_remove_all(result, pattern = ".mzdata")
  resultDirs <- dirname(result)
  result <- basename(result)
  result <- stringr::str_remove_all(result, pattern = "_.*$")
  result <- paste(resultDirs, result, sep = "/")
  result <- unique(result)
  return(result)
}

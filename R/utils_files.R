
# Basic of the files list: a list of three character vectors that hold the file
# locations/names of the different files that are used:
#  - MSPeakFiles : contains peak integration data (chromatogram peaks) of an MS
#    data file
#  - Spectra : contains the spectra belonging to the chromatogram peaks in the
#    MSPeakfiles

#' @title processingFilesEmty
#'
#' @description
#' Helper function function that gives an empty files list
#'
#' @returns a two element list of chracter vectors with the names 'MSPeakFiles'
#'  and 'Spectra'
#'
#' @export
#'
#' @examples
#' processingFilesEmpty()
processingFilesEmpty <- function(){
  list(
    MSPeakFiles = as.character(),
    Spectra = as.character()
  )
}

#' @title emptyFiles
#'
#' @description
#' Helper function that tests if a processingFiles vector is empty
#'
#' @note This function tests if the parameter theFiles is
#'  \code{link[base]{identical}} to the result of the processingFilesEmpty
#'  function. If it is identical, the function will return FALSE. All other
#'  possibilities result in TRUE. The structure of the theFiles parameter is
#'  NOT checked
#'
#' @param theFiles the variable to be tested
#'
#' @returns logical vector
#' @export
#'
#' @examples
#' emptyFiles()
#' processingFiles <- list(
#'   MSPeakFiles = c("WorklistData-0003_MS.csv",
#'                   "WorklistData-0005_MS.csv"),
#'   Spectra = c("WorklistData-0003.mzdata.xml",
#'               "WorklistData-0005.mzdata.xml")
#' )
#' emptyFiles(theFiles = processingFiles)
emptyFiles <- function(theFiles = NA){
  if (!identical(theFiles, NA)){
    if (!identical(theFiles, processingFilesEmpty())){
      return(FALSE)
    }
  }
  return(TRUE)
}

#' @title testCompleteData
#'
#' @description
#' Function that takes a set of filenames and attempts to determine whether
#'  for every MSPeakfiles there is a spectrum file (and vice versa)
#'
#' @param theFilenames two element named list of character vectors
#'
#' @returns named logical vector
#' @export
#'
#' @examples
#' processingFiles <- list(
#'   MSPeakFiles = c("./20250702/WorklistData-0003_MS.csv",
#'                   "./20250702/WorklistData-0005_MS.csv"),
#'   Spectra = c("./20250702/WorklistData-0003.mzdata.xml",
#'               "./20250702/WorklistData-0005.mzdata.xml")
#' )
#' testCompleteData(processingFiles)
#' processingFilesError1 <- list(
#'   MSPeakFiles = c("./20250702/WorklistData-0003_MS.csv"),
#'   Spectra = c("./20250702/20250702/WorklistData-0003.mzdata.xml",
#'               "./20250702/WorklistData-0005.mzdata.xml")
#' )
#' testCompleteData(processingFilesError1)
#' processingFilesError2 <- list(
#'   MSPeakFiles = c("./20250702/WorklistData-0003_MS.csv",
#'                   "./20250702/WorklistData-0005_MS.csv"),
#'   Spectra = c("./20250702/WorklistData-0005.mzdata.xml")
#' )
#' testCompleteData(processingFilesError2)
testCompleteData <- function(theFilenames){
  if (!emptyFiles(theFiles = theFilenames)){
    toTest <- unname(theFilenames[purrr::map_lgl(theFilenames,
                                                 ~length(.x) != 0)])
    if (length(toTest)>0){
      result <- basename(giveFilePathsFromStrings(strings = unname(unlist(theFilenames))))
      result <- unique(result)
      tempResult <- rep(F, length(result))
      for (counter in 1:length(result)){
        tempResult[counter] <- sum(purrr::map_int(theFilenames,
                                                  ~sum(grepl(.x,
                                                             pattern = result[counter])))) == length(theFilenames)
      }
      names(tempResult) <- result
      return(tempResult)
    }
  }
  return(character(0))
}


#' @title gatherRelatedFiles
#'
#' @description
#' function that attempts to get the filenames which 'belong' together, so that
#'  the correct MSPeakFile is matched with the correct Spectra file
#'
#' @param theFiles two element named list of character vectors (filenames to be
#'  matched)
#' @param sample (part of) the name of the file to be matched
#'
#' @returns a list of the filenames in 'theFiles' argument, which have the
#'  'sample' argument
#'
#' @export
#'
#' @examples
#' processingFiles <- list(
#'   MSPeakFiles = c("./20250702/WorklistData-0003_MS.csv",
#'                   "./20250702/WorklistData-0005_MS.csv"),
#'   Spectra = c("./20250702/WorklistData-0003.mzdata.xml",
#'               "./20250702/WorklistData-0005.mzdata.xml")
#' )
#' gatherRelatedFiles(theFiles = processingFiles,
#'                    sample = "WorklistData-0005")
#' gatherRelatedFiles(theFiles = processingFiles,
#'                    sample = "WorklistData-0003")
gatherRelatedFiles <- function(theFiles, sample){
  for (counter in 1:length(theFiles)){
    theFiles[[counter]] <- theFiles[[counter]][grepl(theFiles[[counter]],
                                                     pattern = sample)]
  }
  return(theFiles)
}

#' @title gatherAllCompleteFiles
#'
#' @description
#' Function that extracts the filenames which have matches (and thus have both
#'  MSPeakFiles and Spectra). See examples.
#'
#' @param theFiles two element named list of character vectors (filenames to be
#'  matched)
#'
#' @returns #' @returns a two element list of the filenames in 'theFiles'
#'  argument, which have been matched. Filenames which have not been matched are
#'  excluded
#' @export
#'
#' @examples
#' processingFiles <- list(
#'   MSPeakFiles = c("./20250702/WorklistData-0003_MS.csv",
#'                   "./20250702/WorklistData-0005_MS.csv"),
#'   Spectra = c("./20250702/WorklistData-0003.mzdata.xml",
#'               "./20250702/WorklistData-0005.mzdata.xml")
#' )
#' processingFilesError1 <- list(
#'   MSPeakFiles = c("./20250702/WorklistData-0003_MS.csv"),
#'   Spectra = c("./20250702/WorklistData-0003.mzdata.xml",
#'               "./20250702/WorklistData-0005.mzdata.xml")
#' )
#' processingFilesError2 <- list(
#'   MSPeakFiles = c("./20250702/WorklistData-0003_MS.csv",
#'                   "./20250702/WorklistData-0005_MS.csv"),
#'   Spectra = c("./20250702/WorklistData-0005.mzdata.xml")
#' )
#' gatherAllCompleteFiles(processingFiles)
#' gatherAllCompleteFiles(processingFilesError1)
#' gatherAllCompleteFiles(processingFilesError2)
gatherAllCompleteFiles <- function(theFiles){
  theSamples <- testCompleteData(theFiles)
  theSamples <- theSamples[theSamples]
  if (length(theSamples) != 0){
    theSamples <- names(theSamples)
    result <- processingFilesEmpty()
    for (sampleCounter in 1:length(theSamples)){
      for (counter in 1:length(result)){
        result[[counter]] <- append(result[[counter]], theFiles[[counter]][grepl(theFiles[[counter]], pattern = theSamples[[sampleCounter]])])
      }
    }
    return(result)
  } else {
    return(processingFilesEmpty())
  }
}


#' @title countProcessingData
#'
#' @description
#' Function that returns the number of (complete or incomplete) processing files
#'
#' @param theFilenames two element named list of character vectors (filenames
#'  that may match)
#' @param includeIncomplete logical vector. Default is FALSE, then only complete
#'  processing (matched) files are counter. If TRUE then all (matched and
#'  unmatched) are counter
#'
#' @returns integer vector
#' @export
#'
#' @examples
#' processingFiles <- list(
#'   MSPeakFiles = c("./20250702/WorklistData-0003_MS.csv",
#'                   "./20250702/WorklistData-0005_MS.csv"),
#'   Spectra = c("./20250702/WorklistData-0003.mzdata.xml",
#'               "./20250702/WorklistData-0005.mzdata.xml")
#' )
#' processingFilesError1 <- list(
#'   MSPeakFiles = c("./20250702/WorklistData-0003_MS.csv"),
#'   Spectra = c("./20250702/20250702/WorklistData-0003.mzdata.xml",
#'               "./20250702/WorklistData-0005.mzdata.xml")
#' )
#' countProcessingData(processingFiles)
#' countProcessingData(processingFilesError1)
#' countProcessingData(processingFilesError1, includeIncomplete = TRUE)
countProcessingData <- function(theFilenames, includeIncomplete = FALSE){
  result <- testCompleteData(theFilenames = theFilenames)
  if (includeIncomplete){
    return(length(result))
  } else {
    return(length(result[result]))
  }
}

#' @title getData.Targets
#'
#' @description
#'  Function that reads the peptide targets file
#'
#' @param peptideTargetsFile character vector or data.frame/list with an column/element
#'  called 'file': filename + path to a csv file that contains the peptide target
#'  info. For this function the exact columns and contents are not important. It
#'  needs to have headers and separation character is ';'
#'
#' @returns a data.frame or NA (in case of failure to read)
#'
#' @export
#'
#' @examples
#' demoFile <- fs::path_package(
#'  "extdata",
#'  "targets/targets001.csv",
#'  package = "AgilentDataV2"
#' )
#' demoFile
#' getData.Targets(peptideTargetsFile = demoFile)
getData.Targets <- function(peptideTargetsFile = NA){
  peptideTargets <- NA
  if (!purrr::is_empty(peptideTargetsFile)){
    if (!identical(peptideTargetsFile, NA)){
      if (is.character(peptideTargetsFile)){
        peptideTargets <- readFile(filename = peptideTargetsFile, readFunction = utils::read.table, showWarning = T)(sep = ";", header = T)
      } else { # assumption is that peptideTargetsFile is a data.frame (or list)
        peptideTargets <- peptideTargetsFile$file
      }
    }
  }
  return(peptideTargets)
}

#' @title getData.All
#'
#' @description
#'  Reads all the specified spectra, peak files & targets. Prints messages to
#'  the R console to allow one to follow progress. For use in shiny
#'  applications, there are options to use progress reporters. See
#'  \code{link[shiny]{Progress}} for more information
#'
#' @param sampleName character vector. Name assigned to the resulting
#'  list-element in the listData list
#' @param listData list object, to which the results of this function are added
#'  (as a list element)
#' @param sendMessage function to print progress messages. Default = print. To
#'  prevent messages from being printed, set to e.g.
#'  sendMessage = function(...){}
#' @param updateProgressor argument to pass on a shiny Progress object. See also
#'  \code{link[shiny]{Progress}}. Progress object must be initialized. Default
#'  is NA (no shiny Progress)
#' @param peptideTargetsFile  character vector or data.frame/list with an column/element
#'  called 'file': filename + path to a csv file that contains the peptide target
#'  info. For this function the exact columns and contents are not important. It
#'  needs to have headers and separation character is ';'
#' @param getTargets function that defines how to read the targets file. Default
#'  is \code{link{getData.Targets}}
#' @param mzDataFile mzDataFile filename (including path) of the spectra data
#' @param getSpectra function that defines how to read the spectra data
#'  (mzdata.xml file). Default is \code{link{getData.Spectra}}
#' @param msPeakFile filename (including path) of the peak info file
#' @param getMSPeakFiles function that defines how to read the peak info files.
#'  Default is \code{link{getData.MSPeakFiles}}
#' @param useSmooth logical vector that defines whether smoothed data is removed
#'  (FALSE, default) or if raw data is removed (TRUE). If NA, then no filtering
#'  takes place.
#'
#' @returns listData with added a list element that contains the data read
#'
#' @export
#'
#' @examples
#' demoSpecFile <- fs::path_package(
#'   "extdata",
#'   "raw/WorklistData-0005.mzdata.xml",
#'   package = "AgilentDataV2"
#' )
#' demoPeaksFile <- fs::path_package(
#'   "extdata",
#'   "raw/WorklistData-0005_MS.csv",
#'   package = "AgilentDataV2"
#' )
#' demoTargetsFile <- fs::path_package(
#'   "extdata",
#'   "targets/targets001.csv",
#'   package = "AgilentDataV2"
#' )
#' result <- getData.All(
#'   sampleName = "WKL-0005",
#'   peptideTargetsFile = demoTargetsFile,
#'   mzDataFile = demoSpecFile,
#'   msPeakFile = demoPeaksFile,
#' )
#' result$`WKL-0005`$targets
#' result$`WKL-0005`$sampleName
#' result[[1]]$spectra
#' result[[1]]$peakList
getData.All <- function(sampleName = "Data",
                        listData = list(),
                        sendMessage = print,
                        updateProgressor = NA,
                        peptideTargetsFile = NA,
                        getTargets = getData.Targets,
                        mzDataFile,
                        getSpectra = getData.Spectra,
                        msPeakFile,
                        getMSPeakFiles = getData.MSPeakFiles,
                        useSmooth = FALSE){
  theMessage <- paste0(Sys.time(), " : Starting sample - ", sampleName)
  sendMessage(theMessage)
  listData[[length(listData)+1]] <- list()
  theMessage = " : Reading targets"
  sendMessage(paste0(Sys.time(), theMessage))
  if (is.function(updateProgressor)){
    updateProgressor(step = NULL, message = paste0(sampleName, theMessage))
  }
  listData[[length(listData)]]$sampleName <- sampleName
  listData[[length(listData)]]$mzDataFile <- mzDataFile
  listData[[length(listData)]]$msPeakFile <- msPeakFile
  if (identical(peptideTargetsFile, NA)){
    listData[[length(listData)]]$targetsName <- NA
  } else {
    if (is.character(peptideTargetsFile)){
      listData[[length(listData)]]$targetsName <- tools::file_path_sans_ext(basename(peptideTargetsFile))
    } else {
      listData[[length(listData)]]$targetsName <- tools::file_path_sans_ext(basename(peptideTargetsFile))
    }
  }
  listData[[length(listData)]]$targets <- getTargets(peptideTargetsFile = peptideTargetsFile)
  theMessage <- " : Reading spectra"
  sendMessage(paste0(Sys.time(), theMessage))
  if (is.function(updateProgressor)){
    updateProgressor(step = NULL, message = paste0(sampleName, theMessage))
  }
  listData[[length(listData)]]$spectra <- getSpectra(filename = mzDataFile)
  theMessage <- " : Reading MS peak tables"
  sendMessage(paste0(Sys.time(), theMessage))
  if (is.function(updateProgressor)){
    updateProgressor(step = NULL, message = paste0(sampleName, theMessage))
  }
  listData[[length(listData)]]$peakList <- getMSPeakFiles(filename = msPeakFile, useSmooth = useSmooth)
  names(listData)[[length(listData)]] <- sampleName
  theMessage <- " : Finished sample"
  sendMessage(paste0(Sys.time(), theMessage))
  if (is.function(updateProgressor)){
    updateProgressor(step = NULL, message = paste0(sampleName, theMessage))
  }
  return(listData)
}

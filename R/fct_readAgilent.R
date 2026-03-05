#' @title chromatogramInfo.AgilentExport
#'
#' @description replacement function (factory) for \code{link[readAgilent]{chromatogramInfo.AgilentExport}}
#'  Original implementation had issues with smoothed data. This function does not.
#'
#' @param commentString The description character vector to be converted. Note
#'  that the characters '\' and '#', which are present after the initial file
#'  read (readLines), need to be removed before using this function
#' @param defaultCollapse only used in case of unknown traces. For these, the
#'  resulting elements are joined together into a single character vector which
#'  can be split by this character
#'
#' @returns a data.frame
#'
#' @examples
#' chromatogramInfo.AgilentExport("+ESI TIC Scan Frag=125.0V Data0001.d ")
#' chromatogramInfo.AgilentExport("+ESI BPC Scan Frag=125.0V Data0001.d ")
#' chromatogramInfo.AgilentExport("+ESI EIC(372.8974) Scan Frag=125.0V Data0001.d ")
#' chromatogramInfo.AgilentExport(" +ESI EIC(319.8662) Scan Frag=125.0V WorklistData-0005.d ")
#' chromatogramInfo.AgilentExport("DAD1 - A:Sig=280.0,4.0  Ref=550.0,100.0 Data0001.d")
#' chromatogramInfo.AgilentExport("BinPump1 - A: Pressure Data0001.d")
#'
#' @export
chromatogramInfo.AgilentExport <- function(commentString, defaultCollapse = ";"){
  if (grepl(commentString, pattern = " Smooth")){
    commentString <- stringr::str_remove(commentString, pattern = " Smooth")
    smoothed <- TRUE
  } else {
    smoothed <- FALSE
  }
  tempResult <- unlist(stringr::str_split(commentString, pattern = "\\s"))
  tempResult <- tempResult[nchar(tempResult) !=0 & tempResult != "-"]
  if (grepl(tempResult[1], pattern = "ESI") | grepl(tempResult[1], pattern ="APCI")){
    result <- data.frame(
      polarity = substr(tempResult[1],1,1),
      signal = substr(tempResult[1],2,nchar(tempResult[1])),
      scan = substr(tempResult[2],1,3),
      mz = as.numeric(stringr::str_extract(tempResult[2], pattern = "[:digit:]*\\.[:digit:]*")),
      MS = tempResult[3],
      fragmentor = as.numeric(stringr::str_extract(tempResult[4], pattern = "[:digit:]*\\.[:digit:]*")),
      filename = tempResult[length(tempResult)],
      smooth = smoothed
    )
  } else {
    if (grepl(tempResult[1], pattern = "DAD")){
      result <- data.frame(
        signal = tempResult[1],
        scan = tempResult[2],
        wavelength = as.numeric(stringr::str_extract(tempResult[2], pattern = "[:digit:]*\\.[:digit:]")),
        filename = tempResult[length(tempResult)],
        smooth = smoothed
      )
    } else {
      if (grepl(tempResult[1], pattern = "BinPump")){
        result <- data.frame(
          signal = tempResult[1],
          scan = paste(c(tempResult[3:(length(tempResult)-1)]), collapse = " "),
          filename = tempResult[length(tempResult)],
          smooth = smoothed
        )
      } else {
        result <- data.frame(
          unknown = paste(tempResult, collapse = defaultCollapse)
        )
      }
    }
  }
  return(result)
}

#' @title peakListInfo.AgilentExport
#'
#' @description
#'  Replacement function for \code{link[readAgilent]{readPeaklist.AgilentExport}}
#'  Has an additional argument to allow for different 'translating' of the
#'  description line in peaklist export files
#'
#' @param string character vector to be translated
#' @param defaultCollapse only used in case of unknown traces. For these, the
#'  resulting elements are joined together into a single character vector which
#'  can be split by this character
#' @param translateComment defines the function to be used to translate the
#'  description line. Default is 'chromatogramInfo.AgilentExport2'.
#'
#' @returns a data.frame
#'
#' @examples
#' info <- "D:/MassHunter/Data/Ben/Data0001.d, +ESI TIC Scan Frag=125.0V Data0001.d "
#' peakListInfo.AgilentExport(info)
#' info <- "D:/MassHunter/Data/Ben/Data0001.d, +ESI EIC(591.2807) Scan Frag=125.0V Data0001.d "
#' peakListInfo.AgilentExport(info)
#'
#' @export
peakListInfo.AgilentExport <- function(string, defaultCollapse = ";",
                                      translateComment = chromatogramInfo.AgilentExport){
  string <- unlist(stringr::str_split(string, pattern = ","))
  result <- translateComment(string[2], defaultCollapse = defaultCollapse)
  result$location <-string[1]
  return(result)
}

#' @title readPeaklist.AgilentExport.memory
#'
#' @description replacement function (factory) for \code{link[readAgilent]{readPeaklist.AgilentExport.memory}}
#'  Original implementation did not allow for custom translateComment functions. This one does.
#'
#' @note a peaklist = the result from the integration of traces/chromatograms by
#'  the Agilent Masshunter software
#'
#' @param textLines character vector of the data in Agilent peaklist export
#'  format: first line = description and the rest of the lines is the integration
#'  data. Rownumbers are ignored.
#' @param sep defines the separator for the peaklist data. Default is ','
#' @param startsString character vector that defines which lines are the start
#'  of a peaklist lines in textLines
#' @param translateComment defines the function to be used to translate the
#'  description line. Default is 'chromatogramInfo.AgilentExport'.
#' @param useSmooth logical vector that defines whether smoothed data is removed
#'  (FALSE) or if raw data is removed (TRUE). If NA, then nothing, then no data
#'  is removed. Note: When chromatograms are smoothed, we end up with two
#'  chromatograms per trace: the raw & the smoothed one. If smoothing is done
#'  prior to peak detection/integration, then there will also be two peak tables
#'  per trace. This can lead to confusion in code where data is combined. This
#'  option removes either the smoothed data or the raw data. Default is FALSE
#'
#' @returns function that generates a list of two objects: first element (info)
#'  is data.frame (with info) and second (data) is a list of data.frame's
#'
#' @examples
#' demoFile <- fs::path_package("extdata", "raw/WorklistData-0005_MS.csv", package = "AgilentDataV2")
#' result <- readLines(demoFile)
#' result <- readPeaklist.AgilentExport.memory(result, startsString = "WorklistData-0005")()
#' result$info
#' result$data[[1]]
#' result$data[[3]]
#' @export
readPeaklist.AgilentExport.memory <- function(textLines, sep = ",", startsString,
                                              useSmooth = FALSE,
                                              translateComment = chromatogramInfo.AgilentExport){
  force(textLines)
  force(sep)
  force(startsString)
  force(translateComment)
  force(useSmooth)
  function(...){
    textLines <- purrr::map_chr(textLines, ~stringr::str_replace_all(.x, pattern = "\\\"", replacement = ""))
    textLines <- purrr::map_chr(textLines, ~stringr::str_replace_all(.x, pattern = "\\\\", replacement = "/"))
    starts <- which(purrr::map_lgl(textLines, ~nrow(stringr::str_locate_all(.x, pattern = startsString)[[1]]) == 2))
    results <- purrr::map_df(textLines[starts], ~peakListInfo.AgilentExport(.x,
                                                                            translateComment = translateComment))
    results$start <- starts+1
    results$end <- c(starts[-1]-1, length(textLines))
    pkList <- list()
    for (counter in 1:nrow(results)){
      pkList[[counter]] <- stringr::str_replace_all(textLines[results$start[counter]:results$end[counter]],
                                                    pattern = paste(c(".*",startsString,"\\s,"), collapse = ""),
                                                    replacement = "")
      if (length(pkList[[counter]]) == 1){
        pkList[[counter]] <- NA
      } else {
        pkList[[counter]][1] <- stringr::str_remove_all(
          stringr::str_replace_all(pkList[[counter]][1],
                                   pattern = "%",
                                   replacement = "Perc"),
          pattern = "\\s")
        pkList[[counter]] <- utils::read.csv(text = pkList[[counter]],
                                             sep = ",",
                                             header = T)
        colnames(pkList[[counter]])[1] <- "Filename"
      }
    }
    result <- list(info = results, data = pkList)
    if (!identical(useSmooth, NA)){
      whichInfoKeep <- which((result$info$smooth == useSmooth) | (result$info$scan %in% c("TIC","BPC")))
      result$info <- result$info[whichInfoKeep,]
      result$data <- result$data[whichInfoKeep]
    }
    return(result)
  }
}

#' @title readPeaklist.AgilentExport
#'
#' @description
#'  replacement function (factory) for \code{link[readAgilent]{readPeaklist.AgilentExport}}
#'  Uses 'readFile' function to allow for failed read reads of the file. Note:
#'  will return NA, when reading of the file fails.
#'
#' @note a peaklist = the result from the integration of traces/chromatograms by
#'  the Agilent Masshunter software
#'
#' @param filename name of the peaklist file
#' @param sep defines the separator for the peaklist data. Default is ','
#' @param startsString character vector that defines which lines are the start
#'  of a peaklist lines in the file
#' @param useSmooth logical vector that defines whether smoothed data is removed
#'  (FALSE) or if raw data is removed (TRUE). If NA, then nothing, then no data
#'  is removed. Note: When chromatograms are smoothed, we end up with two
#'  chromatograms per trace: the raw & the smoothed one. If smoothing is done
#'  prior to peak detection/integration, then there will also be two peak tables
#'  per trace. This can lead to confusion in code where data is combined. This
#'  option removes either the smoothed data or the raw data. Default is FALSE
#' @param translateComment defines the function to be used to translate the
#'  description line. Default is 'chromatogramInfo.AgilentExport'
#'
#' @returns function that generates a list of two objects: first element (info)
#'  is data.frame (with info) and second (data) is a list of data.frame's or NA
#'  (when a reading from a file fails)
#'
#' @examples
#' demoFile <- fs::path_package("extdata", "raw/WorklistData-0005_MS.csv", package = "AgilentDataV2")
#' result <- readPeaklist.AgilentExport(demoFile, startsString = "WorklistData-0005")()
#' length(result)
#' result[[1]]
#' result$info
#' result$data[[1]]
#' result$data[[4]]
#'
#' @export
readPeaklist.AgilentExport <- function(filename, sep = ",",
                                       startsString = paste0(stringr::str_remove(tools::file_path_sans_ext(basename(filename)),
                                                                                          pattern = "_.*"), ".d"),
                                       useSmooth = FALSE,
                                       translateComment = chromatogramInfo.AgilentExport){
  force(filename)
  force(sep)
  force(useSmooth)
  force(startsString)
  function(...){
    result <- list()
    textLines <- readFile(filename = filename, readFunction = readLines)()
    if (!identical(textLines, NA)){
      result <- readPeaklist.AgilentExport.memory(textLines = textLines,
                                                  sep = sep,
                                                  startsString = startsString,
                                                  useSmooth = useSmooth,
                                                  translateComment = translateComment)()
    } else {
      result <- NA
    }
    return(result)
  }
}

#' @title combinePeakList.AgilentExport
#'
#' @description combines the results from \code{readPeaklist.AgilentExport.memory}
#'  or \code{readPeaklist.AgilentExport} into a single data.frame.
#'
#' @param peakList list that contains two elements: info (data.frame) and data
#'  (list of data.frame's). Will usually be the result of either \code{readPeaklist.AgilentExport.memory}
#'  or \code{readPeaklist.AgilentExport}
#' @param removeNAColumns logical vector. If TRUE (default) then all columns of
#'  the resulting data.frame that only contain NA's are removed. Default: TRUE
#'
#' @returns a data.frame (or NA)
#'
#' @examples
#' demoFile <- fs::path_package("extdata", "raw/WorklistData-0005_MS.csv", package = "AgilentDataV2")
#' result <- readPeaklist.AgilentExport(demoFile, startsString = "WorklistData-0005")()
#' combinePeakList.AgilentExport(result) |> head(10)
#'
#' @export
combinePeakList.AgilentExport <- function(peakList, removeNAColumns = TRUE){
  if (!identical(peakList, NA)){
    if (nrow(peakList$info) == length(peakList$data)){
      result <- list()
      for (counter in 1:nrow(peakList$info)){
        if (!identical(peakList$data[[counter]], NA)){
          result[[length(result)+1]] <- dplyr::bind_cols(purrr::map_df(1:nrow(peakList$data[[counter]]), ~peakList$info[counter,]),
                                                         peakList$data[[counter]])
        }
      }
      if (length(result) > 0){
        result <- dplyr::bind_rows(result)
        if (removeNAColumns){
          whichNotNA <- purrr::map_lgl(1:ncol(result), ~sum(is.na(result[,.x])) != nrow(result))
          result <- result[, whichNotNA]
        }
        return(result)
      }
    }
  }
  return(NA)
}

#' @title getData.MSPeakFiles
#'
#' @description
#'  wrapper function that reads the peak info file (integration data) and combines
#'  the info & data into a single data.frame
#'
#' @param filename character vector: filename (including path) of the peak info
#'  file
#' @param startsString character vector that defines which lines are the start
#'  of a peaklist lines in the file
#' @param useSmooth logical vector that defines whether smoothed data is removed
#'  (FALSE) or if raw data is removed (TRUE). If NA, then nothing, then no data
#'  is removed. Note: When chromatograms are smoothed, we end up with two
#'  chromatograms per trace: the raw & the smoothed one. If smoothing is done
#'  prior to peak detection/integration, then there will also be two peak tables
#'  per trace. This can lead to confusion in code where data is combined. This
#'  option removes either the smoothed data or the raw data. Default is FALSE
#' @param removeNAColumns logical vector. If TRUE (default) then all columns of
#'  the resulting info data.frame that only contain NA's are removed.
#'  Default: TRUE
#'
#' @returns list or NA (in case of fail)
#'
#' @examples
#' demoFile <- fs::path_package("extdata", "raw/WorklistData-0005_MS.csv", package = "AgilentDataV2")
#' getData.MSPeakFiles(filename = demoFile, startsString = "WorklistData-0005") |> head(10)
#'
#' @export
getData.MSPeakFiles <- function(filename = NA,
                                startsString = paste0(stringr::str_remove(tools::file_path_sans_ext(basename(filename)),
                                                                          pattern = "_.*"), ".d"),
                                useSmooth = FALSE,
                                removeNAColumns = TRUE){
  if (!purrr::is_empty(filename)){
    if (!is.na(filename)){
      result <- readPeaklist.AgilentExport(filename = filename,
                                           startsString = startsString,
                                           useSmooth = useSmooth)()
      result <- combinePeakList.AgilentExport(peakList = result,
                                              removeNAColumns = removeNAColumns)
      return(result)
    }
  }
  return(NA)
}

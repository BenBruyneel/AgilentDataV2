#' @title getRtArea
#'
#' @description function that searches the peak info data for the peak closest
#'  to the Rt (retention time) specified in the targets data.frame. Data
#'  provided needs to contain at least the peak info data in the files section
#'  of the msInfo object of the the parameter 'theData' and the target info in
#'  the 'targets' data.frame of the 'theData' parameter
#'
#' @param theData usually the result from \code{link{getData.All}}
#' @param closeRts list object with tolerances & parameters for peak finding.
#'  See note for description of the different items
#' @param includeMissing default is TRUE. If false then an empty data-row will
#'  be generated when nothing is found. If TRUE then the compound data is in the
#'  data row with NA for all missing data
#' @param mzNr rownumber in the targets data.frame to search the peak info for.
#'  If NA then all rows will be searched individually (may take some time) and
#'  returned in data.frame. If integer value then it will search only for the
#'  'component' in that specific row of the targets data.frame. If a component
#'  is not found, them the peak info will be all NA's
#'
#' @note Items in the closeRts: left & right = tolerances for finding the peak
#'  retention time in the targets table. The function searches first for an EIC
#'  in the peak info data (files section of the msInfo object) that is the exact
#'  same (4 digits) as the one of the component to be searched for. Peaks that
#'  have a retention that falls in between rt - left and rt + right are
#'  considered. If the 'closest' variable is set to TRUE, the function will take
#'  the peak with the Rt closest to the Rt in the targets data.frame. If False,
#'  then if the 'peak' variable is TRUE, the it will sort the found peaks that
#'  fall within the tolerances (rt-left, rt+right) and take the nth peak as
#'  specfified by the 'Peak' column in the targets column. This can be needed to
#'  deal accurately with close together peaks in the same EIC. If the 'peak'
#'  variable is FALSE, then it will select the peak with the highest Area.
#'
#' @note In most cases, it was found that closeRts = (..., ..., FALSE, TRUE)
#'  works best
#'
#' @returns data.frame
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
#' getRtArea(theData = result[[1]], mzNr = 1)
#' getRtArea(theData = result[[1]])
getRtArea <- function(theData,
                      closeRts = list(left = 0.5, right = 0.5,
                                      closest = FALSE, peak = TRUE),
                      includeMissing = TRUE,
                      mzNr = NA){
  if (fieldsPresent(theList = theData,
                    fields = c("targets", "peakList"))){
    if (nrow(theData$peakList)>0){
      themzTable <- theData$targets
      theMS <- theData$peakList
      if (identical(mzNr, NA)){
        themzTable <- purrr::map_df(1:nrow(themzTable), ~getRtArea(theData = theData,
                                                                   closeRts = closeRts,
                                                                   includeMissing = includeMissing,
                                                                   mzNr = .x))
      } else {
        theComponent <- themzTable$Component[mzNr]
        thePeak <- themzTable$Peak[mzNr]
        themzTable <- dplyr::full_join(
          themzTable |>
            dplyr::mutate(mz = formatDigitsLargeNumbers(4)(mz)),
          theMS |>
            dplyr::filter(!is.na(mz)) |>
            dplyr::mutate(mz = formatDigitsLargeNumbers(4)(mz)) |>
            dplyr::select(-c(start, end)),
          by = c("mz" = "mz"),
          relationship = "many-to-many"
        ) |>
          dplyr::filter(Component == theComponent)
        orgmzTable <- themzTable
        themzTable <- themzTable |>
          dplyr::filter(RT >= rt - closeRts[[1]],
                        RT <= rt + closeRts[[2]])
        if (nrow(themzTable)>0){
          themzTable <- themzTable |>
            dplyr::arrange(RT)
          themzTable$peakNr <- 1:nrow(themzTable)
          if (closeRts[[3]]){   # arrange according to distance from rt in table
            themzTable <- themzTable |>
              dplyr::mutate(diffRt = abs(RT - rt)) |>
              dplyr::arrange(diffRt) |>
              dplyr::select(-diffRt) |>
              dplyr::slice(1)
          } else {
            if (!closeRts[[4]]){ # arrange according to found peak Area
              themzTable <- themzTable |>
                dplyr::arrange(dplyr::desc(Area)) |>
                dplyr::slice(1)
            } else { # arrange according to Rt, then select the peak nr
              if (nrow(themzTable)>1){
                themzTable <- themzTable |>
                  dplyr::filter(peakNr == thePeak)
              }
            }
          }
        } else {
          if (includeMissing){
            themzTable <- dplyr::bind_cols(orgmzTable, data.frame(peakNr = as.integer(NA)))
          } else {
            themzTable <- dplyr::bind_cols(themzTable, data.frame(peakNr = as.integer()))
          }
        }
        themzTable <- themzTable |>
          dplyr::rename(foundrt = RT,
                        foundStart = Start,
                        foundEnd = End,
                        foundArea = Area,
                        foundHeight = Height,
                        thePeakinTable = peakNr)
        if (nrow(themzTable)>1){
          themzTable <- themzTable |> dplyr::slice(1)
        }
      }
      return(themzTable)
    }
  }
  return(NA)
}

#' @title getRRtReference
#'
#' @description
#'  Function to find the reference retention from a list-object containing
#'   both a targets table and a peakList table. This depends on the targets
#'   table having a single component being reference = TRUE
#'
#' @param theData usually the result from \code{link{getData.All}}
#' @param closeRts list object with tolerances & parameters for peak finding.
#'  See note for description of the different items
#' @param fixedRrt numeric vector (default is NA). If not NA, then this value is
#'  used as the result of this function. This serves as a means to 'set' a
#'  reference retention time when integration or finding the right peak is
#'  not possible.
#'
#' @note Items in the closeRts: left & right = tolerances for finding the peak
#'  retention time in the targets table. The function searches first for an EIC
#'  in the peak info data (files section of the msInfo object) that is the exact
#'  same (4 digits) as the one of the component to be searched for. Peaks that
#'  have a retention that falls in between rt - left and rt + right are
#'  considered. If the 'closest' variable is set to TRUE, the function will take
#'  the peak with the Rt closest to the Rt in the targets data.frame. If False,
#'  then if the 'peak' variable is TRUE, the it will sort the found peaks that
#'  fall within the tolerances (rt-left, rt+right) and take the nth peak as
#'  specfified by the 'Peak' column in the targets column. This can be needed to
#'  deal accurately with close together peaks in the same EIC. If the 'peak'
#'  variable is FALSE, then it will select the peak with the highest Area.
#'
#' @returns a numeric vector
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
#' getRRtReference(theData = result)
#' getRRtReference(theData = result, fixedRrt = 10)
getRRtReference <- function(theData,
                            closeRts = list(left = 0.5, right = 0.5,
                                            closest = TRUE, peak = FALSE),
                            fixedRrt = NA){
  if (identical(fixedRrt, NA)){
    if (fieldsPresent(theList = theData, fields = "targets")){
      whichNr <- which(theData$targets$Reference)
      result <- getRtArea(theData = theData,
                          closeRts = closeRts,
                          includeMissing = TRUE,
                          mzNr = whichNr)
      return(result$foundrt)
    }
  }
  return(fixedRrt)
}

#' @title getRRtArea
#'
#' @description
#'  Does the same thing as \code{link{getRtArea}}, but works with relative
#'  retention times. It first attempts to find the peak information on the
#'  reference compound in the tergets data.frame and then calculates the
#'  relative retention times with this. These are then used to search the peak
#'  info tables (with the retention times in there also 'converted' to relative
#'  retention times).
#'
#' @param theData usually the result from \code{link{getData.All}}
#' @param closeRts list object with tolerances & parameters for peak finding.
#'  See note with the function \code{link{getRtArea}} for a description of the
#'  different items. Please note that in stead of retention times, for this
#'  function it's actually relative retention times that are used here, so
#'  tolerances/limits should be tighter.
#' @param refcloseRts list object with tolerances & parameters for peak finding
#'  for the reference compound in the targets data.frame (marked by the column
#'  Reference, where ONE compound is set to TRUE)
#'  See note with the function \code{link{getRtArea}} for a description of the
#'  different items. The items are the same as for the closeRts.
#' @param fixedRrt numeric vector (default is NA). If not NA, then this value is
#'  used as the result of this function. This serves as a means to 'set' a
#'  reference retention time when integration or finding the right peak is
#'  not possible.
#' @param includeMissing default is TRUE. If false then an empty data-row will
#'  be generated when nothing is found. If TRUE then the compound data is in the
#'  data row with NA for all missing data
#' @param mzNr rownumber in the targets data.frame to search the peak info for.
#'  If NA then all rows will be searched individually (may take some time) and
#'  returned in data.frame. If integer value then it will search only for the
#'  'component' in that specific row of the targets data.frame. If a component
#'  is not found, them the peak info will be all NA's
#'
#' @note Through testing this function was found to be more dependable, because
#'  it allows some absolute shifts in Rt. If there are very big shifts, it may
#'  be necessary to increase 'left' and 'right' variables in the closeRts
#'  parameter.
#'
#' @returns data.frame
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
#' getRRtArea(theData = result[[1]], mzNr = 1)
#' getRRtArea(theData = result[[1]])
getRRtArea <- function(theData,
                       closeRts = list(left = 0.05, right = 0.05,
                                       closest = TRUE, peak = FALSE),
                       refcloseRts = list(left = 0.5, right = 0.5,
                                          closest = TRUE, peak = FALSE),
                       fixedRrt = NA,# 28.69,
                       includeMissing = TRUE,
                       mzNr = NA){
  if (fieldsPresent(theList = theData,
                    fields = c("targets", "peakList"))){
    referenceRt <- getRRtReference(theData = theData,
                                   closeRts = refcloseRts,
                                   fixedRrt = fixedRrt)
    if (!identical(referenceRt, NA)){
      referenceTargetRt <- theData$targets$rt[theData$targets$Reference]
      theData$targets <- theData$targets |>
        dplyr::mutate(orgrt = rt,
                      rt = rt/referenceTargetRt)
      theData$peakList <- theData$peakList |>
        dplyr::mutate(orgRT = RT,
                      RT = RT/referenceRt)
      result <- getRtArea(theData = theData,
                          closeRts = closeRts,
                          includeMissing = includeMissing,
                          mzNr = mzNr)
      if (!identical(result, NA)){
        result <- result |>
          dplyr::mutate(rt = orgrt,
                        foundrt = orgRT) |>
          dplyr::select(-dplyr::starts_with("org"))
      }
      return(result)
    }
  }
  return(NA)
}

#' @title getMzs
#'
#' @description
#'  Attemps to get the m/z info for all found peaks in the data
#'
#' @param theData usually the result from \code{link{getData.All}}
#' @param thePeaks usually the result from \code{link{getRtArea}} or
#'  \code{link{getRRtArea}}
#' @param mzNr rownumber in the targets data.frame to search the peak info for.
#'  If NA then all rows will be searched individually (may take some time) and
#'  returned in data.frame. If integer value then it will search only for the
#'  'component' in that specific row of the targets data.frame. If a component
#'  is not found, them the peak info will be all NA's
#' @param setSingleRt numeric vector: retention time in minutes to subtract and
#'  add to the retention time of single spectrum spectra. Spectra are usually a
#'  sum or average of a number of spectra close together. The lowest Rt spectrum
#'  provides the start (Rt), the highest Rt spectrum provides the end (Rt). In
#'  case a single spectrum provides the data then start = Rt - setSingleRt and
#'  end = Rt + setSingleRt. This is to accomodate other code where both start
#'  and end are required
#' @param limitPpm how much the m/z in a spectrum is allowed to deviate. All
#'  m/z's inside this range (m/z - ppm, m/z + ppm) are used to generate the
#'  foundmz
#' @param peakIntensityLimit the minimum intensity an m/z peak in a spectrum
#'  must have to used (elimination of noise).
#'
#' @returns thePeaks data.frame with extra column for the m/z (foundmz) (or NA)
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
#' thePeaks <- getRRtArea(theData = result[[1]])
#' getMzs(theData = result[[1]],
#'        thePeaks = thePeaks,
#'        mzNr = 1)
#' \dontrun{
#' getMzs(theData = result[[1]],
#'        thePeaks = thePeaks,
#'        mzNr = NA)
#' }
getMzs <- function(theData,
                   thePeaks = NA,
                   mzNr = 1,
                   setSingleRt = 0.05,
                   limitPpm = c(30,30),
                   peakIntensityLimit = 10)
{
  if (fieldsPresent(theList = theData,
                    fields = c("targets", "spectra"))){
    if (!identical(thePeaks, NA)){
      if (nrow(thePeaks)>0){
        if (identical(mzNr, NA)){
          spectraTable <- purrr::map_df(1:nrow(thePeaks),
                                        ~getMzs(theData = theData,
                                                thePeaks = thePeaks,
                                                mzNr = .x),
                                        limitPpm = limitPpm,
                                        peakIntensityLimit = peakIntensityLimit)
          return(spectraTable)
        } else {
          foundmz <- NA  # in case no peak present
          seekrt <- thePeaks$foundrt[mzNr]
          if (!identical(seekrt, NA)){
            foundSpectrum <- theData$spectra$info(setSingleRt = setSingleRt) |>
              dplyr::mutate(peakNr = 1:theData$spectra$length) |>
              dplyr::select(Start, End, peakNr) |>
              dplyr::filter(Start <= seekrt, End >= seekrt) |>
              dplyr::mutate(middle = Start + ((End-Start)/2),
                            diffMiddle = seekrt - middle) |>
              dplyr::arrange(abs(diffMiddle)) |>
              dplyr::slice(1)
            if (nrow(foundSpectrum)>0){
              foundSpectrum <- foundSpectrum$peakNr
              seekmz <- as.numeric(thePeaks$mz[mzNr])
              mzrange <- calculate.Measured.mz(referenceMz = seekmz,
                                               ppm = c(-limitPpm[1],
                                                       limitPpm[2]))
              mzs <- theData$spectra$spectrum(number = foundSpectrum) |>
                dplyr::filter(mz >= mzrange[1],
                              mz <= mzrange[2],
                              intensity >= peakIntensityLimit)
              foundmz <- stats::weighted.mean(x = as.numeric(formatDigitsLargeNumbers(4)(mzs$mz)),
                                              w = mzs$intensity)
            }
          }
          thePeaks <- thePeaks |>
            dplyr::slice(mzNr)
          thePeaks$foundmz <- foundmz
          return(thePeaks)
        }
      }
    }
  }
  return(NA)
}


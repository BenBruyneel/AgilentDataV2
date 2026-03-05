
#' @title giveLocation
#'
#' @description function to provide modification locations for a peptide
#'  sequence. This is needed as the 'native' peptide for a potential labelling
#'  location does not show a (potential) labelling location in the target table.
#'
#' @param theSequence character vector representing the aminoacid sequence of a
#'  peptide
#'
#' @returns character vector (can be NA)
#'
#' @export
#'
#' @examples
#' giveLocation()
#' giveLocation("DILNQITKPNDVYSFSLASR")
#' giveLocation("VASMASEK")
giveLocation <- function(theSequence = "TQINKVVR"){
  return(
    switch(theSequence,
           "TQINKVVR" = "55",
           "DILNQITKPNDVYSFSLASR" = "92",
           "LYAEERYPILPEYLQCVKELYR" = "122",
           "VTEQESKPVQMMYQIGLFR" = "206",
           "VASMASEKMK" = "226",
           "KIKVYLPR" = "277/279",
           as.character(NA))
  )
}

#' @title giveLabel
#'
#' @description function that correctly queries a modification for containing
#'  DON-label. This is needed for the more complicated situation where there are
#'  two labels on a peptide and the modification (which is a description of
#'  modification(s) present on a peptide) has two elements
#'
#' @param theModification character vector describing the modification(s) on a
#'  peptide
#'
#' @returns character vector, either 'DON' or ''
#' @export
#'
#' @examples
#' giveLabel("DON")
#' giveLabel("DON, DON")
#' giveLabel("DON82")
giveLabel <- function(theModification){
  if (theModification %in% c("DON", "DON, DON", "DON, DON82")){
    return("DON")
  } else {
    return("")
  }
}

#' @title giveImpurity
#'
#' @description function that translates the "DON00" modification to "B" and the
#'  "DON82" modification to "A"
#'
#' @param theModification character vector
#'
#' @returns if theModification is "DON100" then "B", if theModification is
#'  "DON82" then "A". If theModification is something else then "" is returned.
#' @export
#'
#' @examples
#' giveImpurity("DON100")
#' giveImpurity("DON82")
#' giveImpurity("DON")
#' giveImpurity("Nothing")
giveImpurity <- function(theModification){
  return(
    ifelse(grepl(theModification, pattern = "DON100"),
           "B",
           ifelse(grepl(theModification, pattern = "DON82"),
                  "A",
                  "")

    )
  )
}

#' @title completeTable2Signals
#'
#' @description function that converts the result from the function getMzs
#'  (data.frame, which is more or less end result) to data.frame that can be
#'  exported to a .csv file ready to be imported to the correct GxP worksheet in
#'  GxP Signals Notebooks
#'
#' @param theTable data.frame, should be the result from the funciton getMzs
#'
#' @returns a data.frame. If theTable is NA, then the function will return an
#'  empty data.frame
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
#' result <- getMzs(theData = result[[1]],
#'                  thePeaks = thePeaks,
#'                  mzNr = NA)
#' completeTable2Signals(result)
#' completeTable2Signals(NA)
completeTable2Signals <- function(theTable){
  if (!identical(theTable, NA)){
    result <- theTable |>
      dplyr::mutate(Comments = "") |>
      dplyr::select(Control, Sequence, Location, Modification, mz, charge, rt, foundArea, foundrt, foundmz, Comments)
    result$Location <- purrr::map2_chr(result$Sequence,
                                       result$Control,
                                       ~ifelse(.y,
                                               "",
                                               giveLocation(.x)))
    result$Control <- ifelse(result$Control,
                             "Yes", "No")
    result$Label <- purrr::map_chr(result$Modification,
                                   ~giveLabel(.x))
    result$Impurity <- purrr::map_chr(result$Modification,
                                      ~giveImpurity(.x))
    result <- result |>
      dplyr::select(Label, Impurity, dplyr::everything()) |>
      dplyr::mutate(mz = as.numeric(mz),
                    charge = as.integer(charge),
                    rt = as.numeric(rt),
                    foundArea = as.numeric(foundArea),
                    foundrt = as.numeric(foundrt),
                    foundmz = as.numeric(foundmz))
  } else {
    result <- data.frame(
      Label = as.character(),
      Impurity = as.character(),
      Control = as.character(),
      Sequence = as.character(),
      Location = as.character(),
      Modification = as.character(),
      mz = as.numeric(),
      charge = as.integer(),
      rt = as.numeric(),
      foundArea = as.numeric(),
      foundrt = as.numeric(),
      foundmz = as.numeric(),
      Comments = as.character()
    )
  }
  colnames(result)[7:12] <- c("Reference m/z (Da)", "Reference Charge",
                              "Reference Retention Time (min)",
                              "Area", "Retention Time (min)", "m/z (Da)")
  return(result)
}

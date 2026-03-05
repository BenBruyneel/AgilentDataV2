#' @title calculate.ppm
#'
#' @description calculates the mass deviation in ppm (parts per million) between
#'  the measured mass or m/z and a reference mass or m/z (both in Da).
#'
#' @param referenceMz reference mass or m/z in Da. Usually a theoretical mass
#'  calculated from a formula
#' @param measuredMz measured mass or mz in Da.
#'
#' @returns a numeric vector
#' @export
#'
#' @examples
#' calculate.ppm(465.6025, 465.6028)
#' calculate.ppm(465.6025, 465.6025)
#' calculate.ppm(465.6025, 465.7025)
calculate.ppm <- function(referenceMz, measuredMz){
  return(((measuredMz-referenceMz)/referenceMz)*1E6)
}

#' @title calculate.Measured.mz
#'
#' @description calculates the mass (Da) when it deviates a certain ppm (part
#'  per million) from the reference mass (Da).
#'
#' @param referenceMz reference mass or m/z in Da. Usually a theoretical mass
#'  calculated from a formula
#' @param ppm deviation (in ppm) from the reference mass
#'
#' @returns a numeric vector
#' @export
#'
#' @examples
#' calculate.Measured.mz(465.6025, 5)
#' calculate.Measured.mz(465.6025, 0)
#' calculate.Measured.mz(465.6025, -5)
calculate.Measured.mz <- function(referenceMz, ppm){
  return(((ppm+1E6)/1E6)*referenceMz)
}

#' @title Detector Names
#' @description Return names of detectors loaded in BANTER model.
#' 
#' @param x a \code{\link{banter_model}} object.
#' 
#' @return a vector of names.
#' 
#' @author Eric Archer \email{eric.archer@@noaa.gov}
#' 
#' @references Rankin, S. , Archer, F. , Keating, J. L., Oswald, J. N., 
#'   Oswald, M. , Curtis, A. and Barlow, J. (2017), Acoustic classification 
#'   of dolphins in the California Current using whistles, echolocation clicks, 
#'   and burst pulses. Marine Mammal Science 33:520-540. doi:10.1111/mms.12381
#' 
#' @examples
#' data(train.data)
#' # initialize BANTER model with event data
#' bant.mdl <- initBanterModel(train.data$events)
#' # add all detector models
#' bant.mdl <- addBanterDetector(
#'   bant.mdl, train.data$detectors, 
#'   ntree = 50, sampsize = 1, num.cores = 1
#' )
#' getDetectorNames(bant.mdl)
#' 
#' @export
#' 
getDetectorNames <- function(x) names(x@detectors)
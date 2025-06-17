#' @title Sample Size
#' @description Return sample sizes used for a BANTER model.
#' 
#' @param x a \code{\link{banter_model}} object.
#' @param model name of model to extract. Default is \code{"event"} 
#'   to return values for the event-level model. Can also be name of a detector.
#' 
#' @return a vector of sample sizes.
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
#'   ntree = 50, sampsize = 2, num.cores = 1
#' )
#' # run BANTER event model
#' bant.mdl <- runBanterModel(bant.mdl, ntree = 1000, sampsize = 1)
#' 
#' # sample size for the event model
#' getSampSize(bant.mdl)
#' 
#' # sample size for the burst pulse (bp) detector model
#' getSampSize(bant.mdl, "bp")
#' 
#' @export
#' 
getSampSize <- function(x, model = "event") {
  if(model == "event") return(x@sampsize)
  .checkModelName(x, model)
  x@detectors[[model]]@sampsize
}
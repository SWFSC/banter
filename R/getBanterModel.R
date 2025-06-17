#' @title Extract Random Forest Model
#' @description Extract BANTER event or detector Random Forest model.
#' 
#' @param x a \code{\link{banter_model}} object.
#' @param model name of model to extract. Default is \code{"event"} 
#'   to extract the event-level model. Can also be the name of a detector.
#' 
#' @return a \code{\link[randomForest]{randomForest}} model object.
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
#' # run BANTER event model
#' bant.mdl <- runBanterModel(bant.mdl, ntree = 1000, sampsize = 1)
#' 
#' # extract the event randomForest model
#' event.rf <- getBanterModel(bant.mdl)
#' event.rf
#' 
#' # extract the burst pulse (bp) detector model
#' bp.rf <- getBanterModel(bant.mdl, "bp")
#' bp.rf
#' 
#' @export
#' 
getBanterModel <- function(x, model = "event") {
  if(model == "event") return(x@model)
  .checkModelName(x, model)
  x@detectors[[model]]@model
}
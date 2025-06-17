#' @title Detector classification model
#' @description Create classification model for a detector
#'
#' @slot name detector name
#' @slot ids data.frame of event.ids and call.ids for calls in detector
#' @slot model classification model
#' @slot sampsize sample size vector used in model
#' @slot timestamp start and stop times of model run
#'
#' @author Eric Archer \email{eric.archer@@noaa.gov}
#' 
#' @keywords internal
#' 
banter_detector <- methods::setClass(
  "banter_detector",
  slots = c(
    name = "character",
    ids = "data.frame",
    model = "classifier",
    sampsize = "numOrNull",
    timestamp = "dateOrNull"
  )
)

methods::setValidity(
  "banter_detector",
  method = function(object) {
    valid <- NULL
    
    # Check @name -----------------------
    if(length(object@name) != 1) {
      valid <- c(valid, "slot '@name' must have length 1")
    }
    
    # Check @ids ------------------------
    if(!all(c("event.id", "call.id") %in% colnames(object@ids))) {
      valid <- c(valid, "slot '@ids' must have 'event.id' and 'call.id' columns")
    }
    
    if(is.null(valid)) TRUE else valid
  }
)
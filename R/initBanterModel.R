#' @title Initialize BANTER model
#' @description Initialize a BANTER model with event data.
#'
#' @param x a data.frame of events. Every row is a unique event. Must have 
#'   columns named \code{event.id} and \code{species}. All other 
#'   columns will be used as predictor variables for the BANTER event classifier 
#'   model.
#'   
#' @return a \code{\link{banter_model}} object without any detector models.
#' 
#' @note Values in the column \code{species} are passed through the
#'   \code{\link{make.names}} function on creation to ensure they 
#'   don't include invalid characters.
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
#' bant.mdl
#' 
#' @export
#' 
initBanterModel <- function(x) {
  # initialize object
  object <- banter_model(
    data = x, detectors = NULL, 
    model.data = NULL, model = NULL, 
    sampsize = NULL, timestamp = NULL
  )
  
  # check that required columns exist
  if(any(is.na(object@data$event.id))) {
    stop("'x' can't have missing data in the 'event.id' column.")
  }
  if(any(is.na(object@data$species))) {
    stop("'x' can't have missing data in the 'species' column.")
  }
  
  # if duration is present check that there is no missing data
  if("duration" %in% colnames(object@data)) {
    if(any(is.na(object@data$duration))) {
      msg <- paste0(
        "there is missing data in the 'duration' column. ",
        "detector call rates will not be used in the final BANTER model."
      )
      warning(msg)
    }
  }
  
  # check species names
  object@data$species <- make.names(object@data$species, allow_ = FALSE)
  
  object
}
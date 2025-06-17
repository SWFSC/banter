#' @title Number of Events
#' @description Return the number of events in a BANTER model by species.
#' 
#' @param x a \code{\link{banter_model}} object.
#' 
#' @return a data.frame giving the number of events available for each species.
#' @param model name of model to extract. Default is \code{"event"} 
#'   to summarize the event-level model. Can also be name of a detector.
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
#' # number of events in event model
#' numEvents(bant.mdl)
#' 
#' # number of events in burst pulse (bp) detector model
#' numEvents(bant.mdl, "bp")
#' 
#' @importFrom rlang .data
#' 
#' @export
#' 
numEvents <- function(x, model = "event") {
  .checkModelName(x, model)
  df <- if(model == "event") {
    x@data 
  } else {
    x@data |> 
      dplyr::filter(.data$event.id %in% x@detectors[[model]]@ids$event.id)
  }
  spp.fac <- factor(df$species, levels = sort(unique(x@data$species)))
  table(species = spp.fac) |> 
    as.data.frame() |> 
    stats::setNames(c("species", "num.events")) |> 
    dplyr::mutate(species = as.character(.data$species)) 
}

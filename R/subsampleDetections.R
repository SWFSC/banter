#' @title Subsample Detections
#' @description Extract a random subsample of detections for each event and 
#'   detector.
#'
#' @param data a detector data.frame or list of detector data.frames. 
#' @param n a value giving the number (\code{n} >= 1) or 
#'   fraction (\code{n} between 0 and 1) of detections per event per detector 
#'   to select. Detections are randomly selected without replacement. 
#'   If \code{n} is greater than the number of detections in an event, all 
#'   detections for that event will be retained.
#' 
#' @return a detector data.frame or list of detector data.frames with 
#'   no more than \code{n} detections per event per detector.
#' 
#' @author Eric Archer \email{eric.archer@@noaa.gov}
#' 
#' @references Rankin, S., Archer, F., Keating, J. L., Oswald, J. N., 
#'   Oswald, M. , Curtis, A. and Barlow, J. (2017), Acoustic classification 
#'   of dolphins in the California Current using whistles, echolocation clicks, 
#'   and burst pulses. Marine Mammal Science 33:520-540. doi:10.1111/mms.12381
#' 
#' @examples
#' data(train.data)
#' 
#' # initial number of detections per event per detector
#' sapply(train.data$detectors, function(x) table(x$event.id))
#' 
#' # select half of all detectors
#' detect.half <- subsampleDetections(train.data$detectors, 0.5)
#' sapply(detect.half, function(x) table(x$event.id))
#' 
#' # select 20 detections
#' detect.20 <- subsampleDetections(train.data$detectors, 20)
#' sapply(detect.20, function(x) table(x$event.id))
#' 
#' # select 10 detections fro 'ec' detector
#' ec.10 <- subsampleDetections(train.data$detectors$ec, 10)
#' table(ec.10$event.id)
#' 
#' @export
#' 
subsampleDetections <- function(data, n) {
  .subsample <- function(x, n) {
    if(!"event.id" %in% colnames(x)) {
      stop("data.frame must have 'event.id' column.")
    }
    rows <- tapply(1:nrow(x), x$event.id, function(i, n) {
      if(length(i) == 1) return(i) # because sample() with a scalar samples 1:i
      n.i <- if(n < 1) ceiling(length(i) * n) else min(length(i), n)
      sample(i, n.i, replace = FALSE)
    }, n = n, simplify = FALSE)
    x[sort(unlist(rows)), ]
  }
  
  if(is.numeric(n)) {
    if(n <= 0) stop("'n' must be > 0.")
  } else stop("'n' must be numeric.") 
  
  # If data is a list of detectors
  if(methods::is(data, "list")) {
    lapply(data, .subsample, n = n)
  # If data is a data.frame
  } else if(methods::is(data, "data.frame")) {
    .subsample(data, n)
  } else stop("'data' must be a list or data.frame.")
}
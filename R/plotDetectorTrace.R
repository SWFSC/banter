#' @title Plot BANTER Detector Traces
#' @description Plot traces of OOB error rates for detector Random Forest 
#'   models.
#' 
#' @param x a \code{\link{banter_model}} object.
#' @param detector names of models to plot. If set to \code{NULL}, traces for 
#'   all models will be shown.
#' 
#' @seealso \code{\link[rfPermute]{plotTrace}}
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
#' 
#' plotDetectorTrace(bant.mdl)
#' 
#' @export
#' 
plotDetectorTrace <- function(x, detector = NULL) {  
  if(is.null(detector)) detector <- getDetectorNames(x)
  not.found <- setdiff(detector, getDetectorNames(x))
  if(length(not.found) > 0) {
    stop("can't find detectors: ", paste(not.found, collapse = ", "))
  }
  
  traces <- lapply(detector, function(d) {
    rf <- getBanterModel(x, d)
    if(is.null(rf$err.rate)) return(NULL)
    tr <- rfPermute::plotTrace(rf, plot = FALSE)
    tr +
      ggplot2::ylim(c(0, 100)) +
      ggplot2::ggtitle(d) +
      ggplot2::theme(axis.title = ggplot2::element_blank())
  })
  traces <- traces[!sapply(traces, is.null)]
  
  if(length(traces) > 0) {
    traces$ncol <- 1
    traces$bottom <- "Trees"
    traces$left = "Error"
    do.call(gridExtra::grid.arrange, traces)
  } else {
    message("\nNo trace information available - detector models were likely run over multiple cores.")
  }
  
  invisible(NULL)
}
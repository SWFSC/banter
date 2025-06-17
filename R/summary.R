#' @name summary
#' @title BANTER Classifier Model Summary
#' @description Display summaries for event and detector models
#' 
#' @param object a \code{\link{banter_model}} object.
#' @param model name of model to summarize. Default is \code{"event"} 
#'   to summarize the event-level model. Can also be name of a detector.
#' @param n number of final iterations to summarize OOB error rate for. If 
#'   between 0 and 1 is taken as a proportion of chain.
#' @param bins number of bins in inbag histogram.
#' @param ... ignored.
#' 
#' @return In the plot that is created, the upper panel shows the trace of the 
#'   Random Forest model OOB rate across sequential trees in the forest. The
#'   lower plot shows a frequency histogram of the number of times each sample 
#'   was inbag (used as training data in a tree in the forest). The vertical 
#'   red lines indicate the expected inbag rate for samples of each species.
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
#' summary(bant.mdl)
#' 
#' @exportMethod summary
methods::setGeneric("summary")

#' @name summary
#' @rdname summary
#' @method summary banter_model
#' @export 
#' 
summary.banter_model <- function(object, model = "event", n = 0.50, bins = 20, ...) {  
  print(object)
  
  rf <- getBanterModel(object, model)
  if(is.null(rf)) return(invisible(NULL))
  
  message("\n<< Summary for \"", model, "\" model >>")
  
  ntree <- rf$ntree
  cat("\nNumber of trees:", ntree, "\n")
  sampsize <- getSampSize(object, model = model)
  cat("\nSample sizes:\n")
  print(sampsize)
  
  err.rate <- rf$err.rate
  trace <- if(!is.null(err.rate)) {
    if(dplyr::between(n, 0, 1)) n <- ceiling(n * ntree)
    if(n > ntree) n <- ntree
    cat(
      "\nDistribution of percent correctly classified overall in last ", 
      n, " (", round(n / ntree, 2) * 100, "%) trees:\n",
      sep = ""
    )
    correct <- 1 - err.rate[(ntree - n + 1):ntree, "OOB"]
    print(round(summary(correct * 100), 3))
    rfPermute::plotTrace(rf, plot = FALSE) + 
      ggplot2::ylim(c(0, 100))
  } else {
    message("\nNo trace information available - detector models were likely run over multiple cores.")
    NULL
  }
  
  cat("\nSample inbag proportion distribution:\n") 
  print(rbind(
    expected = round(summary(as.vector(sampsize / table(rf$y))), 3) * 100,
    observed = round(summary(1 - (rf$oob.times / ntree)), 3) * 100
  ))
  
  cat("\nConfusion matrix:\n")
  print(rfPermute::confusionMatrix(rf))
  cat("\n")
  
  suppressWarnings({
    inbag <- rfPermute::plotInbag(
      rf, bins = bins, replace = FALSE, sampsize = sampsize, plot = FALSE
    ) +
      ggplot2::xlim(c(0, 100))
    if(is.null(trace)) print(inbag) else {
      gridExtra::grid.arrange(trace, inbag, nrow = 2)
    } 
  })
  
  invisible(NULL)
}

#' @name summary
#' @rdname summary
#' @aliases summary,banter_model-method
methods::setMethod("summary", "banter_model", summary.banter_model) 
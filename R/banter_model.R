#' @title Events classification model
#' @description Create classification model for events
#'
#' @slot data data.frame of event.ids and call.ids for calls in detector
#' @slot detectors list of \code{detector_model} objects
#' @slot model.data data used to create classification model
#' @slot model classification model
#' @slot sampsize sample size vector used in model
#' @slot timestamp start and stop times of model run
#'
#' @author Eric Archer \email{eric.archer@@noaa.gov}
#' 
#' @keywords internal
#' 
banter_model <- methods::setClass(
  "banter_model",
  slots = c(
    data = "data.frame",
    detectors = "listOrNull",
    model.data = "dfOrNull",
    model = "classifier",
    sampsize = "numOrNull",
    timestamp = "dateOrNull"
  )
)

methods::setValidity(
  "banter_model",
  method = function(object) {
    valid <- NULL
    
    # Check @ids ------------------------
    if(!all(c("event.id", "species") %in% colnames(object@data))) {
      valid <- c(valid, "slot '@data' must have 'event.id' and 'species' columns")
    }
    
    # Check @detectors
    if(!is.null(object@detectors)) {
      if(length(obect@detectors) == 0) {
        valid <- c(valid, "slot '@detectors' can't be an empty list")
      } else {
        classes <- unlist(sapply(object@detectors, class))
        if(any(classes != "detector_model")) {
          valid <- c(valid, "all elements in slot '@detectors' must be a 'detector_model' object")
        }
      }
    }
    
    if(is.null(valid)) TRUE else valid
  }
)

methods::setMethod(
  "show",
  "banter_model",
  function(object) {
    df <- numEvents(object) 
    df <- rbind(
      df, 
      data.frame(species = "Overall", num.events = sum(df$num.events))
    )
    
    pct.correct <- modelPctCorrect(object)
    if(!is.null(pct.correct)) {
      df <- dplyr::left_join(df, pct.correct, by = "species")
    }
    
    detectors <- getDetectorNames(object)
    if(length(detectors) > 0) {
      run.time <- sapply(detectors, function(d) {
        start <- object@detectors[[d]]@timestamp["start"]
        end <- object@detectors[[d]]@timestamp["stop"]
        c(format(start), format(end), format(round(difftime(end, start), 2)))
      })
      event.ts <- object@timestamp
      if(!is.null(event.ts)) {
        event.time <- c(
          start = format(event.ts["start"]),
          end = format(event.ts["stop"]),
          run.time = format(
            round(difftime(event.ts["stop"], event.ts["start"]), 2)
          )
        )
        run.time <- cbind(run.time, event = event.time)
      }
      rownames(run.time) <- c("start", "stop", "run.time")
      cat("Model run times:\n")
      print(t(run.time))
      cat("\n")
    }

    cat("Number of events and model classification rate:\n")
    print(df, digits = 4)
  }
)
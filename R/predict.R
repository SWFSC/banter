#' @name predict
#' @title Predict BANTER events
#' @description Predict species of events for novel data from a BANTER model.
#'
#' @param object a \code{\link{banter_model}} object.
#' @param new.data a list of event and detector data that has the same 
#'   predictors as in the \code{banter_model}. It must contain elements called 
#'   \code{events} and \code{detectors}. The \code{events} element must be a 
#'   data.frame that has a column called \code{event.id} and the same
#'   predictor columns as the event data used to initialize the banter model 
#'   (see \code{\link{initBanterModel}}). The \code{detectors} element must be 
#'   a named list with the same detectors used to build the model 
#'   (see \code{\link{addBanterDetector}}).
#' @param ... unused.
#' 
#' @note At least one detector in the model must be present in \code{new.data}. 
#'   Any detectors in the training model that are absent will have all species 
#'   proportions and the the detector propoprtion set to 0. If a column called 
#'   \code{species} is in \code{new.data}, columns for the original species 
#'   designation and if that matches predicted (\code{correct}) will be added 
#'   to the \code{predict.df} data.frame of the output.
#'   
#' @return A list with the following elements: \describe{
#'   \item{events}{the data frame used in the event model for predictions.}
#'   \item{predict.df}{data.frame of predicted species and assignment 
#'      probabilities for each event.}
#'   \item{detector.freq}{data.frame giving the number of events available 
#'      for each detector.}
#'   \item{validation.matrix}{if \code{species} is a column in \code{new.data},
#'      a table giving the classification rate for each event}
#' }
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
#' # predict test data
#' data(test.data)
#' test.pred <- predict(bant.mdl, test.data)
#' test.pred
#' 
#' @exportMethod predict
methods::setGeneric("predict")

#' @name predict
#' @rdname predict
#' @method predict banter_model
#' @export
predict.banter_model <- function(object, new.data, ...) {
  # Check that 'new.data' has elements called 'events' and 'detectors'
  if(!all(c("events", "detectors") %in% names(new.data))) {
    stop(
      "'new.data' must have elements named 'events' and 'detectors'",
      call. = FALSE
    )
  }
  
  # Check that at least one detector is present in 'new.data'
  detectors.found <- intersect(names(object@detectors), names(new.data$detectors))
  if(length(detectors.found) == 0) {
    stop(
      "Can't predict with 'new.data' because no detectors from model are present.", 
      call. = FALSE
    )
  }
  
  # Check that all detectors that are present have the required fields
  for(x in detectors.found) {
    new.cols <- colnames(new.data$detectors[[x]])
    mdl.cols <- names(getBanterModel(object, x)$forest$xlevels)
    cols.missing <- setdiff(mdl.cols, new.cols)
    if(!"event.id" %in% new.cols) cols.missing <- c("event.id", cols.missing)
    if(length(cols.missing) > 0) {
      stop(
        "The '", x, "' detector is missing the following columns: ",
        paste(cols.missing, collapse = ", "),
        call. = FALSE
      )
    }
  }
  
  # Check that event data has all necessary columns
  #    detector columns in event model
  detector.regex <- paste0(
    paste0("^", detectors.found, ".", collapse = "|"),
    paste0(".", detectors.found, "$", collapse = "|"),
    sep = "|"
  )
  #    non-detector columns in event model
  event.cols <- names(getBanterModel(object)$forest$xlevels)
  other.cols <- setdiff(
    event.cols, 
    grep(detector.regex, event.cols, value = TRUE)
  )
  new.cols <- colnames(new.data$events)
  cols.missing <- setdiff(other.cols, new.cols)
  if(!"event.id" %in% new.cols) cols.missing <- c("event.id", cols.missing)
  if(length(cols.missing) > 0) {
    stop(
      "The following columns are missing from the event data: ",
      paste(cols.missing, collapse = ", "),
      call. = FALSE
    )
  }
  
  unique.events <- unique(new.data$events$event.id)
      
  # Get number of calls in each detector for each event
  detector.num <- lapply(names(object@detectors), function(d) {
    if(d %in% names(new.data$detectors)) {
      new.data$events |> 
        dplyr::left_join(new.data$detectors[[d]], by = "event.id") |> 
        dplyr::group_by(.data$event.id) |> 
        dplyr::summarize(n = sum(!is.na(.data$call.id)), .groups = "drop") |> 
        dplyr::mutate(detector = paste0("prop.", d))
    } else { # detector not present in new.data, proportion = 0
      tibble::tibble(
        event.id = unique.events,
        n = 0,
        detector = paste0("prop.", d)
      )
    }
  }) |>
    dplyr::bind_rows()
  
  # Convert number to proportion of calls
  detector.prop <- detector.num |> 
    dplyr::group_by(.data$event.id) |> 
    dplyr::mutate(n = .data$n / sum(.data$n, na.rm = TRUE)) |> 
    dplyr::ungroup() |> 
    tidyr::pivot_wider(names_from = "detector", values_from = "n")
  detector.prop <- replace(detector.prop, is.na(detector.prop), 0) |> 
    as.data.frame()
  
  # Calculate mean votes for each event
  detector.votes <- sapply(names(object@detectors), function(d) {
    if(d %in% names(new.data$detectors)) { # detector present in model
      predict(
        object@detectors[[d]]@model, 
        new.data$detectors[[d]], 
        type = "prob"
      ) |> 
        as.data.frame() |> 
        dplyr::bind_cols(event.id = new.data$detectors[[d]]$event.id)
    } else { # detector not present in model - fill with 0's
      spp <- colnames(object@detectors[[d]]@model$votes)
      vote.0 <- matrix(0, nrow = length(unique.events), ncol = length(spp))
      colnames(vote.0) <- spp
      dplyr::bind_cols(as.data.frame(vote.0), event.id = unique.events)
    }
  }, simplify = FALSE) |> 
    .meanVotes()
  
  # Construct data.frame to predict
  df <- new.data$events |> 
    dplyr::left_join(detector.prop, by = "event.id") |> 
    dplyr::left_join(detector.votes, by = "event.id")
    
    # add call rate columns if duration is present and there is no missing data
    if("duration" %in% colnames(df)) {
      if(all(!is.na(df$duration))) {
        df <- df |> 
          dplyr::left_join(
            detector.num |>  
              dplyr::left_join(
                dplyr::select(df, "event.id", "duration"), 
                by = "event.id"
              ) |> 
              dplyr::mutate(
                detector = gsub("prop.", "rate.", .data$detector),
                n = .data$n / .data$duration
              ) |> 
              dplyr::select(-.data$duration) |> 
              tidyr::pivot_wider(names_from = "detector", values_from = "n"),
            by = "event.id"
          )
      }
    }
  
  is.missing <- sapply(
    setdiff(colnames(df), "species"), 
    function(col) is.na(df[, col])
  )
  df <- df[!apply(is.missing, 1, any), ]

  result <- list(
    events = df,
    predict.df = cbind(
      data.frame(event.id = df$event.id, stringsAsFactors = FALSE),
      predicted = as.character(predict(object@model, df, type = "response")),
      predict(object@model, df, type = "prob"),
      stringsAsFactors = FALSE
    ),
    detector.freq = detector.num |> 
      dplyr::mutate(detector = gsub("prop.", "", .data$detector)) |> 
      dplyr::group_by(.data$detector) |> 
      dplyr::summarize(num.events = sum(.data$n > 0)) |> 
      dplyr::ungroup() |> 
      as.data.frame(stringsAsFactors = FALSE)
  )
  
  if("species" %in% colnames(df)) {
    result$predict.df <- dplyr::mutate(
      result$predict.df,
      original = ifelse(is.na(df$species), NA, make.names(df$species, allow_ = FALSE)),
      correct = ifelse(is.na(df$species), NA, .data$original == .data$predicted)
    )
    conf.df <- result$predict.df
    conf.df$predicted <- factor(
      conf.df$predicted, 
      levels = object@model$classes
    )
    result$validation.matrix <- table(
      original = result$predict.df$original,
      predicted = result$predict.df$predicted
    )
  }
  
  result
}

#' @name predict
#' @rdname predict
#' @aliases predict,banter_model-method
methods::setMethod("predict", "banter_model", predict.banter_model) 

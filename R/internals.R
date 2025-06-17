#' Checks to make sure 'x' contains syntactically valid and unique characters
#' @rdname internals
#' @keywords internal
#' 
.checkValidStrings <- function(x, label) {
  x.valid <- make.names(x)
  not.valid <- x[x != make.names(x, unique = TRUE)]
  if(length(not.valid) > 0) {
    stop(
      "the following values are not valid or are not unique in ",
      label, " : ", paste(not.valid, collapse = ", "),
      ". Use the `make.names()` function to convert.",
      call. = FALSE
    )
  }
  invisible(NULL)
}

#' Checks to make sure 'model' is a valid model name
#' x is a banter_model object
#' @rdname internals
#' @keywords internal
#' 
.checkModelName <- function(x, model) {
  if(model != "event" & !model %in% names(x@detectors)) {
    stop(paste0("model '", model, "' not found."))
  }
  invisible(TRUE)
}

#' Check that detectors are present
#' x is a banter_model object
#' @rdname internals
#' @keywords internal
#' 
.stopIfNoDetectors <- function(x) {
  if(is.null(x@detectors)) {
    stop("no detector models found. see '?addBanterDetector'")
  }
  invisible(TRUE)
}

#' Produces a vector of equal sample sizes for balanced Random Forest model.
#' x is a vector of species or table of species frequencies
#' n is desired sample size
#' @name internals
#' @keywords internal
#' 
.getSampsize <- function(x, n, warn.label) {
  if(n < 0) n <- 1
  freq <- if(is.table(x)) x else table(x)
  
  if(length(freq) < 2) {
    warning(
      warn.label, " has ", length(freq), 
      " species (need at least 2 for model)",
      call. = FALSE, immediate. = TRUE
    )
    return(NULL)
  }
  
  n <- if(n >= 1) n else round(min(freq) * n, 0)
  if(n == 0) n <- 1
  
  bad.freq <- freq[freq <= n]
  good.freq <- freq[freq > n]
  
  if(length(good.freq) < 2) {
    warning(
      warn.label, ":\n",
      "sampsize = ", n,
      " is >= too many species frequencies (need at least 2 for model):\n",
      paste0("  ", names(freq), ": ", freq, "\n", collapse = ""),
      call. = FALSE, immediate. = TRUE
    )
    return(NULL)
  } else if(length(bad.freq) > 0) {
    warning(
      warn.label, ": sampsize = ", n, " is >= species frequencies:\n",
      paste0("  ", names(bad.freq), ": ", bad.freq, "\n", collapse = ""),
      "These species will be used in the model:\n",
      paste0("  ", names(good.freq), ": ", good.freq, "\n", collapse = ""), 
      call. = FALSE, immediate. = TRUE
    ) 
    stats::setNames(rep(n, length(good.freq)), names(good.freq))
  } else {
    stats::setNames(rep(n, length(freq)), names(freq))
  }
}

#' Returns matrix of mean species vote percentage by event
#' x is a list of data.frames with species probability assignments
#' @rdname internals
#' @keywords internal
#' 
.meanVotes <- function(x) {
  df <- sapply(names(x), function(d) {
    x[[d]] |> 
      tidyr::pivot_longer(
        -.data$event.id,
        names_to = "species", 
        values_to = "prob"
      ) |> 
      dplyr::mutate(species = paste0(d, ".", .data$species)) |> 
      dplyr::group_by(.data$event.id, .data$species) |> 
      dplyr::summarize(prob.mean = mean(.data$prob), .groups = "drop") 
  }, simplify = FALSE) |> 
    dplyr::bind_rows() |> 
    tidyr::pivot_wider(names_from = "species", values_from = "prob.mean") 
  
  replace(df, is.na(df), 0)
}

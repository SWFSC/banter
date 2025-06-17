#' @name numCalls
#' @title Number and Proportion of Calls
#' @description Return the number and proportion of calls in BANTER detector 
#'   models.
#' 
#' @param x a \code{\link{banter_model}} object.
#' @param by return summary by \code{"species"} or \code{"event"}.
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
#' # number of calls by species and event
#' numCalls(bant.mdl, "species")
#' numCalls(bant.mdl, "event")
#' 
#' # proportion of calls by species and event
#' propCalls(bant.mdl, "species")
#' propCalls(bant.mdl, "event")
#' 
#' @export
#' 
numCalls <- function(x, by = c("species", "event")) {
  if(is.null(x@detectors)) stop("no detectors loaded in model.")
  by <- switch(match.arg(by), species = "species", event = "event.id")
  df <- lapply(names(x@detectors), function(d) {
    x@data |> 
      dplyr::left_join(x@detectors[[d]]@ids, by = "event.id") |> 
      dplyr::group_by(dplyr::across(by)) |> 
      dplyr::summarize(n = sum(!is.na(.data$call.id)), .groups = "drop") |> 
      dplyr::mutate(detector = paste0("num.", d))
  }) |>
    dplyr::bind_rows() |> 
    tidyr::pivot_wider(names_from = "detector", values_from = "n")
  
  replace(df, is.na(df), 0) |> 
    as.data.frame()
}

#' @rdname numCalls
#' @export
#' 
propCalls <- function(x, by = c("species", "event")) {
  df <- numCalls(x, by)
  by <- colnames(df)[1]
  df |> 
    tidyr::pivot_longer(
      -dplyr::all_of(by),
      names_to = "detector", 
      values_to = "n"
    ) |> 
    dplyr::group_by(dplyr::across(by)) |>
    dplyr::mutate(prop = .data$n / sum(.data$n, na.rm = TRUE)) |> 
    dplyr::ungroup() |> 
    dplyr::select(-.data$n) |> 
    dplyr::mutate(detector = gsub("num.", "prop.", .data$detector)) |> 
    tidyr::pivot_wider(names_from = "detector", values_from = "prop") |> 
    as.data.frame()
}

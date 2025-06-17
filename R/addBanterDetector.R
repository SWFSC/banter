#' @name addBanterDetector
#' @title Add a BANTER Detector Model
#' @description Add a detector model to a BANTER classifier.
#'
#' @param x a \code{\link{banter_model}} object.
#' @param data detector data.frame or named list of detector data.frames. If 
#'   a data.frame, then \code{name} must be provided.
#' @param name detector name.
#' @param ntree number of trees.
#' @param sampsize number or fraction of samples to use in each tree. If < 1, 
#'   then it will be used to select this fraction of the smallest sample size.
#' @param importance retain importance scores in model? Defaults to 
#'   \code{FALSE} and will be ignored if \code{num.cores > 1}.
#' @param num.cores number of cores to use for Random Forest model. Set to 
#'   \code{NULL} to use the maximum number detected on your system - 1.
#' 
#' @return a \code{\link{banter_model}} object with the detector model added or 
#'   removed.
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
#' # initialize BANTER model with event data
#' bant.mdl <- initBanterModel(train.data$events)
#' # add the 'bp' (burst pulse) detector model
#' bant.mdl <- addBanterDetector(
#'   x = bant.mdl, 
#'   data = train.data$detectors$bp, 
#'   name = "bp",
#'   ntree = 50, sampsize = 1, num.cores = 1
#' )
#' bant.mdl
#' 
#' # remove the 'bp' detector model
#' bant.mdl <- removeBanterDetector(bant.mdl, "bp")
#' bant.mdl
#' 
#' @export
#' 
addBanterDetector <- function(x, data, name, ntree, sampsize = 1, 
                              importance = FALSE, num.cores = 1) {
  # Check that detectors is a list
  if(is.null(x@detectors)) x@detectors <- list()
  
  # If data is a list of detectors
  if(methods::is(data, "list")) {
    if(is.null(names(data))) {
      stop("the list of detectors in 'data' must be named.")
    }
    .checkValidStrings(names(data), "detector names")
    new.detectors <- sapply(names(data), function(detector) {
      .checkValidStrings(
        colnames(data[[detector]]), 
        paste0("detector '", detector, "' column names")
      )
      .runDetectorModel(
        x = x, data = data[[detector]], name = detector,
        ntree = ntree, sampsize = sampsize, importance = importance,
        num.cores = num.cores
      )
    }, simplify = FALSE)
    new.detectors <- new.detectors[!sapply(new.detectors, is.null)]
    if(length(new.detectors) > 0) {
      x@detectors[names(new.detectors)] <- new.detectors
    }
  # If data is a data.frame
  } else if(methods::is(data, "data.frame")) {
    if(missing(name)) {
      stop("'name' must be supplied with the 'data' data.frame")
    }
    .checkValidStrings(name, "name")
    .checkValidStrings(colnames(data), "column names")
    new.detector <- .runDetectorModel(
      x = x, data = data, name = name, 
      ntree = ntree, sampsize = sampsize, importance = importance,
      num.cores = num.cores
    )
    if(!is.null(new.detector)) x@detectors[[name]] <- new.detector
  } else stop("'data' must be a list or data.frame.")
  
  # Sort detectors and empty event model info
  if(length(x@detectors) > 1) {
    x@detectors <- x@detectors[order(names(x@detectors))]
  }
  x@model.data <- x@model <- x@sampsize <- x@timestamp <- NULL
  x
}

#' @rdname addBanterDetector
#' @export
#' 
removeBanterDetector <- function(x, name) {
  x@detectors[[name]] <- NULL
  if(length(x@detectors) == 0) x@detectors <- NULL
  x@model.data <- x@model <- x@sampsize <- x@timestamp <- NULL
  x
}

#' @keywords internal
#' 
.runDetectorModel <- function(x, data, name, ntree, 
                              sampsize = 1, importance = FALSE, 
                              num.cores = 1) {
  
  # Check that "event.id" and "call.id" exist
  if(!all(c("event.id", "call.id") %in% colnames(data))) {
    stop("'data' for detector ", name, " must have 'event.id' and 'call.id' columns")
  }
  
  start <- Sys.time()
  
  # Combine event data with call ids in detector
  df <- x@data |> 
    dplyr::select(.data$event.id, .data$species) |> 
    dplyr::inner_join(data, by = "event.id") |> 
    dplyr::mutate(species = as.character(.data$species)) |> 
    as.data.frame()
  
  # Check if any columns need to be removed because of missing data
  to.remove <- sapply(df, function(i) any(is.na(i)))
  if(any(to.remove)) {
    warning(
      "in the '", name, 
      "' detector, missing data found in the following columns: ",
      paste(colnames(df)[to.remove], collapse = ", "), "\n",
      "these columns will not be used in BANTER detector model."
    )
    df <- df[, !to.remove]
  }
  
  # Get and check requested sample size
  sampsize <- .getSampsize(
    df$species,
    sampsize,
    paste0("Detector model '", name, "'")
  )
  if(is.null(sampsize)) return(NULL)

  # Remove missing species and format columns
  df <- df |>
    dplyr::filter(.data$species %in% names(sampsize)) |>
    dplyr::mutate(species = factor(.data$species)) |>
    dplyr::mutate(id = paste0(.data$event.id, ".", .data$call.id)) |>
    tibble::column_to_rownames("id") |>
    as.data.frame() |>
    droplevels()

  # Setup number of cores
  if(is.null(num.cores)) num.cores <- parallel::detectCores() - 1
  num.cores <- min(parallel::detectCores() - 1, num.cores)
  
  # Create random forest parameter list
  params <- list(
    predictors = df |> 
      dplyr::select(-.data$event.id, -.data$call.id, -.data$species),
    response = df$species,
    sampsize = sampsize,
    importance = importance & num.cores == 1
  )
  
  cl <- swfscMisc::setupClusters(num.cores)
  rf <- tryCatch({
    if(is.null(cl)) { # Don't parallelize if num.cores == 1
      params$ntree <- ntree
      .rfFuncDetector(params) 
    } else { # Parallel random forest
      # Create multicore rf function
      params$ntree <- ceiling(ntree / num.cores)
      parallel::clusterEvalQ(cl, require(randomForest))
      parallel::clusterExport(cl, "params", environment())
      rf.list <- parallel::parLapplyLB(
        cl, 1:num.cores, function(i, p) .rfFuncDetector(p), p = params
      )
      do.call(randomForest::combine, rf.list)
    }
  }, finally = if(!is.null(cl)) parallel::stopCluster(cl) else NULL)
  
  # Load banter_detector object
  banter_detector(
    name = name,
    ids = df[, c("event.id", "call.id")], 
    model = rf,
    sampsize = sampsize,
    timestamp = c(start = start, stop = Sys.time())
  ) 
}

#' Detector randomForest function
#' @rdname internals
#' @keywords internal
#' 
.rfFuncDetector <- function(params) {
  randomForest::randomForest(
    x = params$predictors, 
    y = params$response, 
    ntree = params$ntree, 
    sampsize = params$sampsize, 
    replace = FALSE, 
    importance = params$importance, 
    proximity = FALSE, 
    norm.votes = FALSE
  )
}

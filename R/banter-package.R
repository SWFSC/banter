#' banter
#' 
#' @name banter-package
#' @aliases banter
#' @title BioAcoustic EveNT ClassifiER
#' @references Rankin, S. , Archer, F. , Keating, J. L., Oswald, J. N., 
#'   Oswald, M. , Curtis, A. and Barlow, J. (2017), Acoustic classification 
#'   of dolphins in the California Current using whistles, echolocation clicks,
#'   and burst pulses. Marine Mammal Science 33:520-540. doi:10.1111/mms.12381
#' @importFrom rlang .data
#' @importFrom methods new
#' @keywords package
NULL
"_PACKAGE"

.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    "Welcome to banter v", utils::packageVersion("banter"), "\n",
    "See `banterGuide()` for a tutorial."
  )
}

setOldClass(c("randomForest", "randomForest.formula", "rfPermute", "ranger"))
setClassUnion("classifier", c("randomForest", "randomForest.formula", "rfPermute", "ranger", "NULL"))
setClassUnion("listOrNull", c("list", "NULL"))
setClassUnion("dfOrNull", c("data.frame", "NULL"))
setClassUnion("dateOrNull", c("POSIXct", "NULL"))
setClassUnion("numOrNull", c("numeric", "NULL"))

#' @docType data
#' @name train.data
#' @title Training events and detectors
#' @description A list of events and call data from detectors for training 
#'   BANTER model
#' @usage data(train.data)
#' @format list
#' @keywords datasets
NULL

#' @docType data
#' @name test.data
#' @title Testing events and detectors
#' @description A list of events and call data from detectors for testing 
#'   BANTER model
#' @usage data(test.data)
#' @format list
#' @keywords datasets
NULL

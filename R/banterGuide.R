#' @title BANTER Guide
#' @description Open a browser window displaying 
#'   "BANTER: A User’s Guide to Acoustic Classification".
#' 
#' @author Eric Archer \email{eric.archer@@noaa.gov}
#' 
#' @references Rankin, S. , Archer, F. , Keating, J. L., Oswald, J. N., 
#'   Oswald, M. , Curtis, A. and Barlow, J. (2017), Acoustic classification 
#'   of dolphins in the California Current using whistles, echolocation clicks,
#'   and burst pulses. Marine Mammal Science 33:520-540. doi:10.1111/mms.12381
#' 
#' @export
#' 
banterGuide <- function() {
  utils::browseURL("https://taikisan21.github.io/PAMpal/banterGuide.html")
  #utils::browseURL(system.file("banterGuide.html", package = "banter"))
}
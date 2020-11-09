#' @import salso
.onAttach <- function(libname, pkgname) {
  packageStartupMessage("The 'sdols' package has been supplanted by the 'salso' package.")
}
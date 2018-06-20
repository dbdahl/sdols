#' @import commonsMath

.onLoad <- function(libname, pkgname) {
  assign.callback <- function(s) s %@% '
    import org.ddahl.sdols._
    import org.ddahl.sdols.network._
    import org.ddahl.sdols.clustering._
    import org.ddahl.sdols.featureallocation._
    import org.apache.commons.math3.random.{ RandomDataGenerator => RDG }
  '
  .rscalaPackage(c(pkgname,"commonsMath"),assign.callback)
}

.onUnload <- function(libpath) {
  .rscalaPackageUnload()
}


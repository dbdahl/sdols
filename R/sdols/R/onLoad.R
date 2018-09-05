#' @import commonsMath

.onLoad <- function(libname, pkgname) {
  s <- scala(c(pkgname,"commonsMath"))
  scalaLazy(function(s) s + '
    import org.ddahl.sdols._
    import org.ddahl.sdols.network._
    import org.ddahl.sdols.clustering._
    import org.ddahl.sdols.featureallocation._
    import org.apache.commons.math3.random.{ RandomDataGenerator => RDG }
  ')
  scalaUnserializeRegister(scalaUnserialize.clusterings)
  assign("s",s,envir=parent.env(environment()))
}

.onUnload <- function(libpath) {
  close(s)
}

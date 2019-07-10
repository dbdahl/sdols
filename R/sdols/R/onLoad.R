#' @import rscala
#' @import commonsMath
#'
.onLoad <- function(libname, pkgname) {
  s <- scala("commonsMath")  # Don't need to include sdols since it imports commonsMath.
  scalaLazy(function(s) s + '
    import org.ddahl.sdols._
    import org.ddahl.sdols.network._
    import org.ddahl.sdols.clustering._
    import org.ddahl.sdols.featureallocation._
    import org.apache.commons.math3.random.{ RandomDataGenerator => RDG }

    def rdg() = {
      val ints = R.evalI1("runif(2,-.Machine$integer.max,.Machine$integer.max)")
      val seed = ((ints(0).asInstanceOf[Long]) << 32) | (ints(1) & 0xffffffffL)
      val r = new RDG()
      r.reSeed(seed)
      r
    }
  ')
  scalaPushRegister(scalaPush.clustering,"clustering",s)
  scalaPullRegister(scalaPull.clustering,"clustering",s)
  scalaPushRegister(scalaPush.featureAllocation,"featureAllocation",s)
  scalaPullRegister(scalaPull.featureAllocation,"featureAllocation",s)
  assign("s",s,envir=parent.env(environment()))
}

.onUnload <- function(libpath) {
  close(s)
}


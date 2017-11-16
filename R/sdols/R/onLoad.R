.onLoad <- function(libname, pkgname) {
  snippet <- '
    import org.ddahl.sdols._
    import org.ddahl.sdols.network._
    import org.ddahl.sdols.partition._
    import org.ddahl.sdols.featureallocation._
    import org.apache.commons.math3.random.{ RandomDataGenerator => RDG }
  '
  ## Tell users they may want to set heap.maximum using options(rscala.heap.maximum="4G")
  .rscalaPackage(pkgname,classpath.packages=c("commonsMath"),snippet=snippet)
}

.onUnload <- function(libpath) {
  .rscalaPackageUnload()
}


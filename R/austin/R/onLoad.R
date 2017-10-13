.onLoad <- function(libname, pkgname) {
  snippet <- '
    import org.ddahl.austin._
    import org.ddahl.austin.network._
    import org.apache.commons.math3.random.{ RandomDataGenerator => RDG }
  '
  ## Tell users they may want to set heap.maximum using options(rscala.heap.maximum="4G")
  .rscalaPackage(pkgname,classpath.packages=c("commonsMath","shallot"),snippet=snippet)
}

.onUnload <- function(libpath) {
  .rscalaPackageUnload()
}


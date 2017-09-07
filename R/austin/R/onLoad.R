.onLoad <- function(libname, pkgname) {
  snippet <- '
    import org.ddahl.austin.network._
    import org.apache.commons.math3.random.{ RandomDataGenerator => RDG }
  '
  .rscalaPackage(pkgname,classpath.packages=c("commonsMath","shallot"),snippet=snippet)   ## Tell users to set heap.maximum using options(rscala.heap.maximum="4G")
}


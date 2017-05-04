.onLoad <- function(libname, pkgname) {
  snippet <- '
    import org.ddahl.austin._
    import org.apache.commons.math3.random.{ RandomDataGenerator => RDG }
  '
  .rscalaPackage(pkgname,snippet=snippet)   ## Tell uses to set heap.maximum using options("rscala.command.line.options"="-J-Xmx4g")
}


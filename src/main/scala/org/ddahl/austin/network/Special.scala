package org.ddahl.austin.network

import org.apache.commons.math3.special.Gamma.regularizedGammaQ
import org.apache.commons.math3.special.Gamma.{gamma => Gamma}
import org.apache.commons.math3.util.FastMath.{exp, pow}
import wilx.math.exponential.integrals.ExponentialIntegrals.{Exponential_Integral_Ei => expint}

object Special {

  // This implementation allows for a negative first argument.
  def incompleteGamma(a: Double, x: Double): Double = {
    if ( a > 0 ) {
      Gamma(a) * regularizedGammaQ(a,x)
    } else if ( a < 0 ) { // This branch could be made more efficient.
      (-pow(x,a)*exp(-x)+incompleteGamma(a+1,x))/a
    } else {
      -expint(-x)
    }
  }

}

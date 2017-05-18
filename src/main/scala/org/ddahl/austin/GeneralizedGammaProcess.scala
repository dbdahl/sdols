package org.ddahl.austin

import org.apache.commons.math3.util.FastMath.{exp, pow}
import org.apache.commons.math3.special.Gamma.{gamma => Gamma, regularizedGammaQ}

case class GeneralizedGammaProcess(alpha: Double, kappa: Double, gamma: Double) {

  if (alpha <= 0) new IllegalArgumentException("alpha must be greater than 0.")
  if (kappa < 0) new IllegalArgumentException("kappa must be greater than or equal to 0.")
  if ((gamma < 0) || (gamma >= 1)) new IllegalArgumentException("gamma must be greater than or equal to 0 and less than 1.")

  private def upperIncompleteGamma(s: Double, x: Double) = {
    println(s)
    println(x)
    Gamma(s) * regularizedGammaQ(s,x)
  }

  private def levyMeasure(w: Double) = exp(-kappa*w) / ( pow(w,1+gamma) * Gamma(1-gamma) )

  def levyIntensity(w: Double): Double = kappa match {
    case 0.0 =>
      println("Hi1")
      1.0 / ( pow(w,gamma) * Gamma(1-gamma) * gamma )
    case _ =>
      println("Hi2")
      pow(kappa,gamma) * upperIncompleteGamma(-gamma,kappa*w) / Gamma(1-gamma)
  }

  def sample(rbase: (Int) => Array[Double]): Unit = {

  }

}

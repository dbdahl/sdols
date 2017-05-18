package org.ddahl.austin

import org.apache.commons.math3.util.FastMath.{exp, pow}
import org.apache.commons.math3.special.Gamma.{gamma => Gamma}
import org.apache.commons.math3.analysis.UnivariateFunction
import org.apache.commons.math3.analysis.integration.SimpsonIntegrator

case class GeneralizedGammaProcess(alpha: Double, kappa: Double, gamma: Double) {

  if (alpha <= 0) new IllegalArgumentException("alpha must be greater than 0.")
  if (kappa < 0) new IllegalArgumentException("kappa must be greater than or equal to 0.")
  if ((gamma < 0) || (gamma >= 1)) new IllegalArgumentException("gamma must be greater than or equal to 0 and less than 1.")

  private lazy val integrator = new SimpsonIntegrator

  private val func = new UnivariateFunction {
    def value(w: Double): Double = exp(-kappa*w) * pow(w,-1-gamma)
  }

  var maxEval = 1000

  def levyIntensity(a: Double, b: Double): Double = {
    val v = integrator.integrate(maxEval, func, a, b) * alpha / Gamma(1-gamma)
    println(integrator.getEvaluations)
    println(integrator.getIterations)
    v
  }

  def sample(rbase: (Int) => Array[Double]): Unit = {

  }

}

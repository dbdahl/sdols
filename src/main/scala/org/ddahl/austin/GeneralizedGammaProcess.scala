package org.ddahl.austin

import org.apache.commons.math3.util.FastMath.{exp, pow}
import org.apache.commons.math3.special.Gamma.{regularizedGammaQ, gamma => Gamma}
import org.apache.commons.math3.analysis.UnivariateFunction
import org.apache.commons.math3.analysis.integration.SimpsonIntegrator
import org.apache.commons.math3.analysis.solvers.BrentSolver
import org.apache.commons.math3.distribution.PoissonDistribution
import org.apache.commons.math3.random.RandomGenerator
import wilx.math.exponential.integrals.ExponentialIntegrals.{Exponential_Integral_Ei => expint}

import scala.reflect.ClassTag

case class GeneralizedGammaProcess(alpha: Double, kappa: Double, gamma: Double) {

  // Constraints in Caron & Fox (2017).
  require(alpha>0, "alpha must be greater than 0.")
  require(kappa>=0, "kappa must be greater than or equal to 0.")
  require(gamma<1, "gamma must be less than 1.")
  if ( kappa == 0 ) require(gamma > 0, "gamma must be greater than 0 when kappa is 0.")

  // Extra constraint to make things interesting...
  require(gamma>=0, "gamma must be greater than or equal to zero for there to be an infinite number of jumps.")

  def intensity(x: Double): Double = exp(-kappa*x) * pow(x,-1-gamma)

  def intensityTailArea(x: Double): Double = {
    require(x>=0,"x must be greater than or equal to 0.")
    alpha * ( kappa match {
      case 0.0 =>
        pow(x,-gamma) / ( Gamma(1-gamma) * gamma )
      case _ =>
        pow(kappa,gamma) * incompleteGamma(-gamma,kappa*x) / Gamma(1-gamma)
    } )
  }

  def intensityIntervalArea(a: Double, b: Double): Double = {
    require(a<b, "a must be less than b.")
    intensityTailArea(a) - intensityTailArea(b)
  }

  def sampleWeights(smallestWeight: Double, maxTailArea: Double, coarseness: Double, rng: RandomGenerator): Array[Double] = {
    var x = smallestWeight
    var mm = coarseness
    val solver = new BrentSolver()
    val func = new UnivariateFunction {
      override def value(t: Double): Double = exp(-kappa*x) - coarseness*exp(-kappa*t*x)*pow(t,-1-gamma)
    }
    var list = List[Double]()
    while ( intensityTailArea(x) > maxTailArea ) {
      mm = solver.solve(100, func, 1.0, mm)
      val left = x
      val right = x*mm
      val rate = intensityIntervalArea(left,right)
      val poisson = new PoissonDistribution(rng, rate, PoissonDistribution.DEFAULT_EPSILON, PoissonDistribution.DEFAULT_MAX_ITERATIONS)
      val n = poisson.sample()
      if ( n > 0 ) {
        val rd = new RampDistribution(left,right,intensity(left),intensity(right))
        val draws = Array.ofDim[Double](n)
        var i = 0
        while (i < n) {
          val proposal = rd.sample(rng)
          val acceptanceProbability = intensity(proposal)/rd.envelopFunction(proposal)
          if ( rng.nextDouble() <= acceptanceProbability ) {
            draws(i) = proposal
            i += 1
          }
        }
        list = draws.sorted ++: list
      }
      x *= mm
    }
    list.toArray
  }

  def mkMeasure[A](weights: Array[Double], baseSampler: () => A)(implicit classTag: ClassTag[A]): Array[(Double, A)] = {
    weights.zip(Array.fill(weights.length)(baseSampler()))
  }

  def mkMeasure[A](weights: Array[Double], atoms: Array[A]): Array[(Double,A)] = {
    weights.zip(atoms)
  }

  def intensityIntervalAreaSlow(a: Double, b: Double, maxEval: Int): Double = {
    require(a<b, "a must be less than b.")
    val integrator = new SimpsonIntegrator
    val func = new UnivariateFunction {
      def value(x: Double): Double = intensity(x)
    }
    integrator.integrate(maxEval, func, a, b) * alpha / Gamma(1-gamma)
  }

  private def incompleteGamma(a: Double, x: Double): Double = {
    if ( a > 0 ) {
      Gamma(a) * regularizedGammaQ(a,x)
    } else if ( a < 0 ) {
      (-pow(x,a)*exp(-x)+incompleteGamma(a+1,x))/a
    } else {
      -expint(-x)
    }
  }

}

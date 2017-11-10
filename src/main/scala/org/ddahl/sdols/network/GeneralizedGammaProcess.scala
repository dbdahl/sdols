package org.ddahl.sdols.network

import Special.incompleteGamma
import org.apache.commons.math3.util.FastMath.{exp, pow, log}
import org.apache.commons.math3.special.Gamma.{gamma => Gamma}
import org.apache.commons.math3.analysis.UnivariateFunction
import org.apache.commons.math3.distribution.PoissonDistribution
import org.apache.commons.math3.random.RandomGenerator
import scala.reflect.ClassTag

case class GeneralizedGammaProcess(alpha: Double, kappa: Double, gamma: Double) {

  // Constraints in Caron & Fox (2017).
  require(alpha > 0, "alpha must be greater than 0.")
  require(kappa >= 0, "kappa must be greater than or equal to 0.")
  require(gamma < 1, "gamma must be less than 1.")
  if (kappa == 0) require(gamma > 0, "gamma must be greater than 0 when kappa is 0.")

  private val cachedValue0 = 1.0 / Gamma(1-gamma)
  private val cachedValue1 = if (kappa == 0.0) cachedValue0 / gamma else Double.NaN
  private val cachedValue2 = if (kappa != 0.0) cachedValue0 * pow(kappa, gamma) else Double.NaN

  def intensity(x: Double): Double = cachedValue0 * exp(-kappa * x) * pow(x, -1 - gamma)

  def intensityTailArea(x: Double): Double = {
    require(x >= 0, "x must be greater than or equal to 0.")
    alpha * (kappa match {
      case 0.0 =>
        cachedValue1 * pow(x, -gamma)
      case _ =>
        cachedValue2 * incompleteGamma(-gamma, kappa * x)
    })
  }

  def intensityIntervalArea(a: Double, b: Double): Double = {
    require(a < b, s"a ($a) must be less than b ($b).")
    intensityTailArea(a) - intensityTailArea(b)
  }

  def sampleWeights(smallestWeight: Double, nBreaks: Int, rng: RandomGenerator): Array[Double] = {
    var list = List[Double]()
    // Sample weights less than 1.0 using rejection sampling based on my "RampDistribution" a total of nBreaks times.
    for ( i <- 0 until nBreaks ) {
      val left = smallestWeight + i*(1-smallestWeight)/nBreaks
      val right = smallestWeight + (i+1)*(1-smallestWeight)/nBreaks
      val rate = intensityIntervalArea(left, right)
      if ( rate == Double.PositiveInfinity ) throw new RuntimeException("smallestWeight is too small for these parameter settings.")
      val poisson = new PoissonDistribution(rng, rate, PoissonDistribution.DEFAULT_EPSILON, PoissonDistribution.DEFAULT_MAX_ITERATIONS)
      val n = poisson.sample()
      if (n > 0) {
        val rd = RampDistribution(left, right, intensity(left), intensity(right))
        val draws = Array.ofDim[Double](n)
        var i = 0
        while (i < n) {
          val proposal = rd.sample(rng)
          val acceptanceProbability = intensity(proposal) / rd.envelopFunction(proposal)
          if (rng.nextDouble() <= acceptanceProbability) {
            draws(i) = proposal
            i += 1
          }
        }
        list = draws.sortWith(_>_) ++: list
      }
    }
    // Sample weights greater than or equal to 1.0 using rejection sampling based on an exponential distribution.
    val rate = intensityTailArea(1.0)
    val poisson = new PoissonDistribution(rng, rate, PoissonDistribution.DEFAULT_EPSILON, PoissonDistribution.DEFAULT_MAX_ITERATIONS)
    val n = poisson.sample()
    if ( n > 0 ) {
      val probGreaterThan1 = exp(-kappa)
      val draws = Array.ofDim[Double](n)
      var i = 0
      while (i < n) {
        val candidate = {
          var c = 0.0
          while (c < 1.0) c = -log(rng.nextDouble()) / kappa
          c
        }
        val ratio = probGreaterThan1 * pow(candidate, -1 - gamma)
        if (rng.nextDouble <= ratio) {
          draws(i) = candidate
          i += 1
        }
      }
      list = draws.sortWith(_>_) ++: list
    }
    list.toArray
  }

  def sample[A](baseSampler: () => A, smallestWeight: Double, nBreaks: Int, rng: RandomGenerator)(implicit classTag: ClassTag[A]): Array[(Double, A)] = {
    val weights = sampleWeights(smallestWeight, nBreaks, rng)
    sample(baseSampler, weights)
  }

  def sample[A](baseSampler: () => A, weights: Array[Double])(implicit classTag: ClassTag[A]): Array[(Double, A)] = {
    val atoms = Array.fill(weights.length)(baseSampler())
    measure(atoms, weights)
  }

  def measure[A](atoms: Array[A], weights: Array[Double]): Array[(Double, A)] = {
    weights.zip(atoms)
  }

  // Kept for testing purposes to validate the better intensityIntervalArea.
  def intensityIntervalAreaSlow(a: Double, b: Double, maxEval: Int): Double = {
    require(a < b, s"a ($a) must be less than b ($b).")
    val integrator = new org.apache.commons.math3.analysis.integration.SimpsonIntegrator()
    val func = new UnivariateFunction {
      def value(x: Double): Double = intensity(x)
    }
    integrator.integrate(maxEval, func, a, b) * alpha
  }

}

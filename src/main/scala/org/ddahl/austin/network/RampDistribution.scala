package org.ddahl.austin.network

import org.apache.commons.math3.random.RandomGenerator
import org.apache.commons.math3.util.FastMath.sqrt

case class RampDistribution(lower: Double, upper: Double, fLower: Double, fUpper: Double) {

  require(lower < upper, "Lower bound must be less than the upper bound.")

  val slope = ( fUpper - fLower ) / ( upper - lower )
  val normalizingConstant = 2 / ((upper-lower) * (slope * (upper-lower) + 2*fLower))

  def envelopFunction(x: Double): Double = x match {
    case w if w < lower || w > upper => 0.0
    case _ =>
      fLower + slope*(x-lower)
  }

  def densityFunction(x: Double): Double = envelopFunction(x) * normalizingConstant

  def distributionFunction(x: Double): Double = x match {
    case w if w < lower => 0.0
    case w if w > upper => 1.0
    case _ =>
      ((x-lower) * (slope * (x-lower) + 2 * fLower)) / 2 * normalizingConstant
  }

  def quantileFunction(y: Double): Double = {
    require(0 <= y && y <= 1, "The probability y must be in [0,1].")
    val aa = slope
    val bb = 2*(fLower-lower*slope)
    val cc = slope*lower*lower - 2*fLower*lower - y*2/normalizingConstant
    val tmp = sqrt(bb*bb-4*aa*cc)
    val x1 = (-bb - tmp)/(2*aa)
    val x2 = (-bb + tmp)/(2*aa)
    if ( x1 < lower || x1 > upper ) x2
    else x1
  }

  def sample(random: RandomGenerator): Double = quantileFunction(random.nextDouble())

}

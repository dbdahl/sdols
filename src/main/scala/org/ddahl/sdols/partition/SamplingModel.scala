package org.ddahl.sdols
package partition

trait SamplingModel2[A] {

  type tipe = A

  def sample(): A

  def sample(subset: Subset[A]): A

  def copy(x: A): A = x

  def logDensity(i: Int, subset: Subset[A]): Double

}

class GeneralNullSamplingModel2[A] extends SamplingModel2[A] {

  def sample(): A = null.asInstanceOf[A]

  def sample(subset: Subset[A]) = null.asInstanceOf[A]

  def logDensity(i: Int, subset: Subset[A]): Double = 0.0

}

object NullSamplingModel2 extends GeneralNullSamplingModel2[Null]


package org.ddahl.sdols
package featureallocation

import org.ddahl.commonsmath._

object FeatureAllocationSummary {

  def expectedPairwiseAllocationMatrix[A](fas: Seq[FeatureAllocation[A]]): Array[Array[Double]] = {
    if ( fas.isEmpty ) throw new IllegalArgumentException("'candidates' cannot be empty.")
    val nItems = fas.head.nItems
    val zero = MatrixFactory(nItems,nItems)
    (fas.par.aggregate(zero)((sum,x) => sum + x.pairwiseAllocationMatrix,_+_) :/ fas.size).getData
  }

  def sumOfSquares[A](fa: FeatureAllocation[A], pam: Array[Array[Double]]): Double = {
    (MatrixFactory(fa.pairwiseAllocationMatrix) - pam).map(x => x*x).sum
  }

  def leastSquares[A](candidates: Seq[FeatureAllocation[A]], pamOption: Option[Array[Array[Double]]] = None): FeatureAllocation[A] = {
    if ( candidates.isEmpty ) throw new IllegalArgumentException("'candidates' cannot be empty.")
    val pam = pamOption.getOrElse(expectedPairwiseAllocationMatrix(candidates))
    candidates.par.minBy(sumOfSquares(_,pam))
  }

}

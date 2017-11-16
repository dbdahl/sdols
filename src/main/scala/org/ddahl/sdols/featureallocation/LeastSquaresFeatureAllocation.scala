package org.ddahl.sdols.featureallocation

import org.ddahl.commonsmath._

object LeastSquaresFeatureAllocation {

  def sharedFeaturesMatrix[A](fas: Seq[FeatureAllocation[A]]): Array[Array[Double]] = {
    if ( fas.isEmpty ) throw new IllegalArgumentException("'candidates' cannot be empty.")
    val nItems = fas.head.nItems
    val zero = MatrixFactory(nItems,nItems)
    (fas.par.aggregate(zero)((sum,x) => sum + x.sharedFeaturesMatrix,_+_) :/ fas.size).getData
  }

  def sumOfSquares[A](fa: FeatureAllocation[A], sfm: Array[Array[Double]]): Double = {
    (MatrixFactory(fa.sharedFeaturesMatrix) - sfm).map(x => x*x).sum
  }

  def apply[A](candidates: Seq[FeatureAllocation[A]], sfmOption: Option[Array[Array[Double]]] = None): FeatureAllocation[A] = {
    if ( candidates.isEmpty ) throw new IllegalArgumentException("'candidates' cannot be empty.")
    val sfm = sfmOption.getOrElse(sharedFeaturesMatrix(candidates))
    candidates.par.minBy(sumOfSquares(_,sfm))
  }

}


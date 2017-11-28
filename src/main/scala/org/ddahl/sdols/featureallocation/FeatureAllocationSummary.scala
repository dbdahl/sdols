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

  def binderSumOfSquares[A](fa: FeatureAllocation[A], pam: Array[Array[Double]]): Double = {
    (MatrixFactory(fa.pairwiseAllocationMatrix) - pam).map(x => x*x).sum
  }

  def minBinderAmongDraws[A](candidates: Seq[FeatureAllocation[A]], pamOption: Option[Array[Array[Double]]] = None): FeatureAllocation[A] = {
    if ( candidates.isEmpty ) throw new IllegalArgumentException("'candidates' cannot be empty.")
    val pam = pamOption.getOrElse(expectedPairwiseAllocationMatrix(candidates))
    candidates.par.minBy(binderSumOfSquares(_,pam))
  }

  private def expand(fa: FeatureAllocation[Null], availableFeatures: List[Feature[Null]], i: Int): Seq[FeatureAllocation[Null]] = {
    if ( availableFeatures.isEmpty ) Seq()
    else {
      val faPlus = fa.add(i,availableFeatures.head)
      val tail = availableFeatures.tail
      if ( tail.isEmpty ) Seq(fa,faPlus)
      else expand(fa,tail,i) ++ expand(faPlus,tail,i)
    }
  }

  private def sequentiallyAllocatedLatentStructureOptimization(initial: FeatureAllocation[Null], maxSize: Int, permutation: List[Int], pamTransform: Array[Array[Double]], lossEngine: (FeatureAllocation[Null],Array[Array[Double]]) => Double): FeatureAllocation[Null] = {
    var fa = initial
    for ( i <- permutation ) {
      val candidates = expand(fa,fa.toList,i)
      val candidates2 = if ( ( maxSize <= 0 ) || ( fa.size < maxSize ) ) {
        fa.add(i,null) +: candidates   // DBD: Maybe add more.
      } else candidates
      fa = candidates2.minBy(lossEngine(_,pamTransform))
    }
    fa
  }

  def sequentiallyAllocatedLatentStructureOptimization(nCandidates: Int, pam: Array[Array[Double]], maxSize: Int, loss: String): FeatureAllocation[Null] = {
    val (lossEngine, pamTransform) = loss match {
      case "binder" => (binderSumOfSquares[Null] _, pam)
    }
    val rng = new scala.util.Random()
    val nItems = pam.length
    val ints = List.tabulate(nItems) { identity }
    val empty = FeatureAllocation.empty[Null](nItems)
    val candidates = Range(0,nCandidates).par.map { i =>
      val permutation = rng.shuffle(ints)
      sequentiallyAllocatedLatentStructureOptimization(empty,maxSize,permutation,pamTransform,lossEngine)
    }
    candidates.minBy(lossEngine(_,pamTransform))
  }

}


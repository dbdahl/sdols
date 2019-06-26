package org.ddahl.sdols
package featureallocation

import org.apache.commons.math3.linear.Array2DRowRealMatrix
import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.collection.mutable.ListBuffer
import scala.collection.parallel.immutable.ParVector

object FeatureAllocationSummary {

  implicit val ordering = CrossCompatibility.doubleOrdering

  def expectedPairwiseAllocationMatrix[A](fas: Seq[FeatureAllocation[A]]): Array[Array[Double]] = {
    if ( fas.isEmpty ) throw new IllegalArgumentException("'candidates' cannot be empty.")
    val nItems = fas.head.nItems
    val zero = new Array2DRowRealMatrix(nItems,nItems)
    ParVector(fas:_*).aggregate(zero)((sum,x) => sum.add(new Array2DRowRealMatrix(x.pairwiseAllocationMatrix.map(_.map(_.toDouble)),false)), _.add(_)).scalarMultiply(1.0/fas.size).getData
  }

  def sumOfSquaresSlow[A](fa: FeatureAllocation[A], pam: Array[Array[Double]]): Double = {
    new Array2DRowRealMatrix(fa.pairwiseAllocationMatrix.map(_.map(_.toDouble)),false).subtract(new Array2DRowRealMatrix(pam,false)).getDataRef.foldLeft(0.0)(_+_.foldLeft(0.0)((a,b) => a + b*b))
  }

  def sumOfAbsolutesSlow[A](fa: FeatureAllocation[A], pam: Array[Array[Double]]): Double = {
    new Array2DRowRealMatrix(fa.pairwiseAllocationMatrix.map(_.map(_.toDouble))).subtract(new Array2DRowRealMatrix(pam,false)).getDataRef.foldLeft(0.0)(_+_.foldLeft(0.0)((a,b) => a + b.abs))
  }

  private def engine[A](counts: Array[Array[Int]], pam: Array[Array[Double]], f: Double => Double): Double = {
    val nItems = pam.length
    var sum1 = 0.0
    var sum2 = 0.0
    var i = 0
    while ( i < nItems ) {
      val countsi = counts(i)
      val pami = pam(i)
      sum1 += f(countsi(i) - pami(i))
      var j = 0
      while ( j < i ) {
        sum2 += f(countsi(j) - pami(j))
        j += 1
      }
      i += 1
    }
    sum1 + 2*sum2
  }

  def sumOfSquares[A](fa: FeatureAllocation[A], pam: Array[Array[Double]]): Double = {
    engine(fa.pairwiseAllocationTriangle,pam,(x: Double) => x*x)
  }

  def sumOfAbsolutes[A](fa: FeatureAllocation[A], pam: Array[Array[Double]]): Double = {
    engine(fa.pairwiseAllocationTriangle,pam,(x: Double) => x.abs)
  }

  def minAmongDraws[A](candidates: Seq[FeatureAllocation[A]], maxSize: Int, multicore: Boolean, loss: String, pamOption: Option[Array[Array[Double]]]): FeatureAllocation[A] = {
    if ( candidates.isEmpty ) throw new IllegalArgumentException("'candidates' cannot be empty.")
    val pam = pamOption.getOrElse(expectedPairwiseAllocationMatrix(candidates))
    val lossEngine = getLoss[A](loss)
    val iter = (if ( multicore ) ParVector(candidates:_*) else candidates).iterator
    iter.minBy { featureAllocation =>
      if ( ( maxSize > 0 ) && ( featureAllocation.size > maxSize ) ) Double.PositiveInfinity
      else lossEngine(featureAllocation, pam)
    }
  }

  private def expand(fa: FeatureAllocation[Null], availableFeatures: List[Feature[Null]], i: Int): ListBuffer[FeatureAllocation[Null]] = {
    if ( availableFeatures.isEmpty ) ListBuffer()
    else {
      val faPlus = fa.add(i,availableFeatures.head)
      val tail = availableFeatures.tail
      if ( tail.isEmpty ) ListBuffer(fa,faPlus)
      else expand(fa,tail,i) ++ expand(faPlus,tail,i)
    }
  }

  private def padWithFeature(fa: FeatureAllocation[Null], f: Feature[Null], nTimes: Int) = {
    var fa2 = fa
    var j = 0
    while ( j < nTimes ) {
      fa2 = fa2.add(f)
      j += 1
    }
    fa2
  }

  private def sequentiallyAllocatedLatentStructureOptimization(initial: FeatureAllocation[Null], maxSize: Int, permutation: List[Int], pamTransform: Array[Array[Double]], lossEngine: (FeatureAllocation[Null],Array[Array[Double]]) => Double): FeatureAllocation[Null] = {
    val max = if ( maxSize <= 0 ) Int.MaxValue else maxSize
    var fa = initial
    for ( i <- permutation ) {
      val singleton = Feature(null,i)
      val expectedNumberOfFeatures = pamTransform(i)(i).round.toInt
      fa = if ( fa.isEmpty ) padWithFeature(fa, singleton, expectedNumberOfFeatures min max)
      else {
        val candidates = expand(fa, fa.toList, i)
        val residualCapacity = maxSize - fa.nFeatures
        val candidates2 = if ( residualCapacity > 0 ) candidates.map { fa2 =>
          padWithFeature(fa2, singleton, ( expectedNumberOfFeatures - fa2.nFeatures(i) ) min residualCapacity )
        } else candidates
        candidates2.minBy(lossEngine(_, pamTransform))
      }
    }
    fa
  }

  private def getLoss[A](loss: String) = {
    loss match {
      case "squaredError" => sumOfSquares[A] _
      case "absoluteError" => sumOfAbsolutes[A] _
    }
  }

  def sequentiallyAllocatedLatentStructureOptimization(nCandidates: Int, budgetInSeconds: Double, pam: Array[Array[Double]], maxSize: Int, maxScans: Int, multicore: Boolean, loss: String): (FeatureAllocation[Null], Int, Int) = {
    val lossEngine = getLoss[Null](loss)
    val rng = new scala.util.Random()   // Thread safe!
    val nItems = pam.length
    val ints = List.tabulate(nItems) { identity }
    val empty = FeatureAllocation.empty[Null](nItems)
    val nCores = if ( multicore ) Runtime.getRuntime.availableProcessors else 1
    val nCandidatesPerThread = (nCandidates - 1) / nCores + 1
    val budgetInMillis = if ( budgetInSeconds <= 0 ) Long.MaxValue else budgetInSeconds * 1000L
    val start = System.currentTimeMillis
    val futures = List.fill(nCores) { Future {
      var counter = 0
      var minScore = Double.PositiveInfinity
      var best: (FeatureAllocation[Null], Int) = (null,-1)
      while ( (counter < nCandidatesPerThread) && ( System.currentTimeMillis - start <= budgetInMillis ) ) {
        counter += 1
        val permutation = rng.shuffle(ints)
        val candidate = (sequentiallyAllocatedLatentStructureOptimization(empty,maxSize,permutation,pam,lossEngine),-1)
        val score = lossEngine(candidate._1,pam)
        if ( score < minScore ) {
          minScore = score
          best = candidate
        }
      }
      (best._1, best._2, minScore, counter)
    }}
    val seq = Await.result(Future.sequence(futures), Duration.Inf)
    val nCandidatesInPractice = seq.foldLeft(0)( (sum,tuple) => sum + tuple._4 )
    val best = seq.minBy(_._3)
    (best._1, best._2, nCandidatesInPractice)
  }

}


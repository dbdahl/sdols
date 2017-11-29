package org.ddahl.sdols
package partition

import org.ddahl.commonsmath._
import org.apache.commons.math3.util.FastMath.log

object PartitionSummary {

  // Assumes that 'clusterings' is a non-empty sequence of partitions.
  def expectedPairwiseAllocationMatrix[A](clusterings: Seq[Partition[A]]): Array[Array[Double]] = {
    val nItems = clusterings(0).nItems
    val x = Array.ofDim[Int](nItems,nItems)
    clusterings.foreach { partition =>
      partition.foreach { subset =>
        val y = subset.toArray
        var i = 0
        while ( i < y.length ) {
          val ii = y(i)
          var j = i+1
          while ( j < y.length ) {
            val jj = y(j)
            x(ii)(jj) += 1
            x(jj)(ii) += 1
            j += 1
          }
          x(ii)(ii) += 1
          i += 1
        }
      }
    }
    val cl: Double = clusterings.length
    x.map(_.map(_/cl))
  }

  // Assumes that 'clusterings' is a non-zero-length array of arrays of equal lengths.
  def expectedPairwiseAllocationMatrix(clusterings: Array[Array[Int]]): Array[Array[Double]] = {
    val n = clusterings(0).length
    val x = Array.ofDim[Int](n,n)
    var k = 0
    while ( k < clusterings.length ) {
      val p = clusterings(k)
      var i = 0
      while ( i < n ) {
        val xi = x(i)
        val pi = p(i)
        var j = 0
        while ( j < n ) {
          if ( pi == p(j) ) xi(j) += 1
          j += 1
        }
        i += 1
      }
      k += 1
    }
    val cl: Double = clusterings.length
    x.map(_.map(_/cl))
  }

  def confidenceComputations[A](clustering: Array[Int], pam: Array[Array[Double]]): (Array[Int], Array[Double], Array[Array[Double]], Array[Int], Array[Int]) = {
    val nItems = pam.length
    assert(clustering.length == nItems)
    def overlap(subset1: Set[Int], subset2: Set[Int]): Double = {
      subset1.map(i => {
        val pami = pam(i)
        subset2.map(j => {
          pami(j)
        }).sum
      }).sum / (subset1.size * subset2.size)
    }
    val partition = makePartition(clustering)
    val matrix = scala.collection.mutable.HashMap[Tuple2[Set[Int], Set[Int]], Double]()
    partition.foreach(s1 => partition.foreach(s2 => matrix((s1, s2)) = overlap(s1, s2)))
    val subsetsWithSumOverlap = partition.map(s1 => (s1, partition.map(s2 => matrix(s1, s2)).sum))
    val sortedClusters = subsetsWithSumOverlap.sortWith((t1, t2) => t1._2 < t2._2).map(_._1)
    val map = sortedClusters.zipWithIndex.toMap
    val matrixOutSmall = Array.ofDim[Double](partition.size, partition.size)
    partition.foreach(c1 => partition.foreach(c2 => matrixOutSmall(map(c1))(map(c2)) = matrix((c1, c2))))
    val confidence = new Array[Double](nItems)
    for (i <- 0 until nItems) {
      val ppmi = pam(i)
      val subset = partition.find(_.contains(i)).get
      confidence(i) = subset.map(j => ppmi(j)).sum / subset.size
    }
    val UNINITIALIZED = -1
    val exemplar = Array.fill(map.size) { UNINITIALIZED }
    val order = Array.range(0, nItems).sortWith{ (i, j) =>
      val io = map(partition.find(_.contains(i)).get)
      val jo = map(partition.find(_.contains(j)).get)
      if (io < jo) {
        true
      } else if (io > jo) {
        false
      } else {
        val iBigger = confidence(i) > confidence(j)
        if (iBigger) {
          if ((exemplar(io) == UNINITIALIZED) || (confidence(exemplar(io)) < confidence(i))) exemplar(io) = i
        } else {
          if ((exemplar(jo) == UNINITIALIZED) || (confidence(exemplar(jo)) < confidence(j))) exemplar(jo) = j
        }
        iBigger
      }
    }
    val labels = Array.range(0, nItems).map(i => map(partition.find(_.contains(i)).get))
    (labels, confidence, matrixOutSmall, order, exemplar)
  }

  @scala.annotation.tailrec
  private def makePartition(labelsWithIndex: Iterable[(Int, Int)], list: List[Set[Int]]): List[Set[Int]] = {
    val label = labelsWithIndex.head._1
    val (left, right) = labelsWithIndex.partition(_._1 == label)
    val longerList = left.map(_._2).toSet +: list
    if (right.isEmpty) longerList
    else makePartition(right, longerList)
  }

  private def makePartition(clustering: Array[Int]): List[Set[Int]] = {
    if (clustering.isEmpty) throw new IllegalArgumentException("Labels may not by empty.")
    makePartition(clustering.zipWithIndex, List[Set[Int]]())
  }

  def lowerBoundVariationOfInformation[A](partition: Partition[A], pam: Array[Array[Double]]): Double = {
    val nItems = partition.nItems
    var sum1 = 0.0
    var i = 0
    while ( i < nItems ) {
      var sum2 = 0
      var sum3 = 0.0
      var sum4 = 0.0
      var j = 0
      while ( j < nItems ) {
        sum2 += (if (partition.paired(i, j)) 1 else 0)
        sum3 += pam(i)(j)
        sum4 += (if (partition.paired(i, j)) pam(i)(j) else 0.0)
        j += 1
      }
      sum1 += log(2, sum2) + log(2, sum3) - 2*log(2, sum4)
      i += 1
    }
    sum1/nItems
  }

  private def lowerBoundVariationOfInformationEngine[A](partition: Partition[A], pam: Array[Array[Double]]): Double = {
    var sum = 0.0
    val subsets = partition.toArray
    var k = 0
    while ( k < subsets.length ) {
      val subset = subsets(k).toArray
      sum += subset.length * log(2, subset.length)
      var sum2 = 0.0
      var ii = 0
      while ( ii < subset.length ) {
        var sum3 = 0.0
        val pamii = pam(subset(ii))
        var jj = 0
        while ( jj < subset.length ) {
          sum3 += pamii(subset(jj))
          jj += 1
        }
        sum2 += log(2,sum3)
        ii += 1
      }
      sum -= 2*sum2
      k += 1
    }
    sum
  }

  def binderSumOfAbsolutesSlow[A](partition: Partition[A], pam: Array[Array[Double]]): Double = {
    (MatrixFactory(partition.pairwiseAllocationMatrix) - pam).map(_.abs).sum
  }

  def binderSumOfSquaresSlow[A](partition: Partition[A], pam: Array[Array[Double]]): Double = {
    (MatrixFactory(partition.pairwiseAllocationMatrix) - pam).map(x => x*x).sum
  }

  private def binderOffset[A](pam: Array[Array[Double]], f: Double => Double) = {
    var offset = 0.0
    var i = 1
    while ( i < pam.length ) {
      val pi = pam(i)
      var j = 0
      while ( j < i ) {
        offset += f(pi(j))
        j += 1
      }
      i += 1
    }
    2*offset
  }

  def binderSumOfAbsolutes[A](partition: Partition[A], pam: Array[Array[Double]]): Double = {
    val pamTransform = pam.map(_.map(x => 2-4*x))
    binderOffset(pam,(x: Double) => x.abs) + binderEngine(partition,pamTransform)
  }

  def binderSumOfSquares[A](partition: Partition[A], pam: Array[Array[Double]]): Double = {
    val pamTransform = pam.map(_.map(x => 2-4*x))
    binderOffset(pam,(x: Double) => x*x) + binderEngine(partition,pamTransform)
  }

  private def binderEngine[A](partition: Partition[A], pamTransform: Array[Array[Double]]): Double = {
    var sum = 0.0
    partition.foreach { subset =>
      val y = subset.toArray
      var i = 0
      while ( i < y.length ) {
        val xx = pamTransform(y(i))
        var j = i+1
        while ( j < y.length ) {
          sum += xx(y(j))
          j += 1
        }
        i += 1
      }
    }
    sum
  }

  def minBinderAmongDraws(candidates: Array[Array[Int]], pamOption: Option[Array[Array[Double]]] = None): Partition[Null] = {
    minBinderAmongDraws(candidates.map(Partition.apply),pamOption)
  }

  def minBinderAmongDraws[A](candidates: Seq[Partition[A]], pamOption: Option[Array[Array[Double]]]): Partition[A] = {
    if ( candidates.isEmpty ) throw new IllegalArgumentException("'candidates' cannot be empty.")
    val pam = pamOption.getOrElse(expectedPairwiseAllocationMatrix(candidates))
    val pamTransform = pam.map(_.map(x => 0.5-x))
    candidates.par.minBy(binderEngine(_, pamTransform))
  }

  private def sequentiallyAllocatedLatentStructureOptimization(initial: Partition[Null], maxSize: Int, permutation: List[Int], pamTransform: Array[Array[Double]], lossEngine: (Partition[Null],Array[Array[Double]]) => Double): Partition[Null] = {
    var partition = initial
    for ( i <- permutation ) {
      val candidates = partition.map(subset => partition.add(i,subset)).toList
      val candidates2 = if ( ( maxSize <= 0 ) || ( partition.size < maxSize ) ) {
        partition.add(Subset(null,i)) :: candidates
      } else candidates
      partition = candidates2.minBy(lossEngine(_,pamTransform))
    }
    partition
  }

  def sequentiallyAllocatedLatentStructureOptimization(nCandidates: Int, pam: Array[Array[Double]], maxSize: Int, loss: String): Partition[Null] = {
    val (lossEngine, pamTransform) = loss match {
      case "binder" | "squaredError" | "absoluteError" => (binderEngine[Null] _, pam.map(_.map(x => 0.5-x)))
      case "vi" => (lowerBoundVariationOfInformationEngine[Null] _, pam)
    }
    val rng = new scala.util.Random()
    val nItems = pam.length
    val ints = List.tabulate(nItems) { identity }
    val empty = Partition.empty[Null]()
    val candidates = Range(0,nCandidates).par.map { i =>
      val permutation = rng.shuffle(ints)
      sequentiallyAllocatedLatentStructureOptimization(empty,maxSize,permutation,pamTransform,lossEngine)
    }
    candidates.minBy(lossEngine(_,pamTransform))
  }

}


package org.ddahl.sdols
package partition

import org.ddahl.commonsmath._

object PartitionSummary {

  // Assumes that 'clusterings' is a non-empty sequence of partitions.
  def expectedPairwiseClusteringMatrix[A](clusterings: Seq[Partition[A]]): Array[Array[Double]] = {
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
  def expectedPairwiseClusteringMatrix(clusterings: Array[Array[Int]]): Array[Array[Double]] = {
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

  def confidenceComputations[A](clustering: Array[Int], pcm: Array[Array[Double]]): (Array[Int], Array[Double], Array[Array[Double]], Array[Int], Array[Int]) = {
    val nItems = pcm.length
    assert(clustering.length == nItems)
    def overlap(subset1: Set[Int], subset2: Set[Int]): Double = {
      subset1.map(i => {
        val pcmi = pcm(i)
        subset2.map(j => {
          pcmi(j)
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
      val ppmi = pcm(i)
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

  /*
  def sumOfSquaresSlow[A](partition: Partition[A], pcm: Array[Array[Double]]): Double = {
    (MatrixFactory(partition.pairwiseClusteringMatrix) - pcm).map(x => x*x).sum
  }
  */

  def sumOfSquares[A](partition: Partition[A], pcm: Array[Array[Double]]): Double = {
    val offset = pcm.map(_.map(x => x*x).sum).sum - pcm.length
    val pcmTransform = pcm.map(_.map(x => 2-4*x))
    offset + sumOfSquaresEngine(partition,pcmTransform)
  }

  private def sumOfSquaresEngine[A](partition: Partition[A], pcmTransform: Array[Array[Double]]): Double = {
    var sum = 0.0
    partition.foreach { subset =>
      val y = subset.toArray
      var i = 0
      while ( i < y.length ) {
        val xx = pcmTransform(y(i))
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

  def leastSquares[A](candidates: Seq[Partition[A]], pcmOption: Option[Array[Array[Double]]] = None): Partition[A] = {
    if ( candidates.isEmpty ) throw new IllegalArgumentException("'candidates' cannot be empty.")
    val pcm = pcmOption.getOrElse(expectedPairwiseClusteringMatrix(candidates))
    val pcmTransform = pcm.map(_.map(x => 0.5-x))
    candidates.par.minBy(sumOfSquaresEngine(_, pcmTransform))
  }

  private def forwardOptimization(permutation: List[Int], pcmTransform: Array[Array[Double]]): Partition[Null] = {
    var partition = Partition.empty[Null]()
    for ( i <- permutation ) {
      val candidates = partition.add(Subset(null,i)) :: partition.map { subset =>
        partition.add(i,subset)
      }.toList
      partition = candidates.minBy(sumOfSquaresEngine(_,pcmTransform))
    }
    partition
  }

  def forwardOptimization(nCandidates: Int, pcm: Array[Array[Double]]): Partition[Null] = {
    val rng = new scala.util.Random()
    val nItems = pcm.length
    val ints = List.tabulate(nItems) { identity }
    val pcmTransform = pcm.map(_.map(x => 0.5-x))
    val candidates = Range(0,nCandidates).par.map { i =>
      val permutation = rng.shuffle(ints)
      forwardOptimization(permutation, pcmTransform)
    }
    candidates.minBy(sumOfSquaresEngine(_,pcmTransform))
  }

}


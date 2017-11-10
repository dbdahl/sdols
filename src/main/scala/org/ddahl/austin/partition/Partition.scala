package org.ddahl.austin.partition

import scala.annotation.tailrec

object Partition {

  // Assumes that 'clusterings' is a non-zero-length array of arrays of equal lengths.
  def pairwiseProbabilityMatrix(clusterings: Array[Array[Int]]): Array[Array[Double]] = {
    val n = clusterings(0).length
    val x = Array.ofDim[Double](n,n)
    val unit = 1.0 / clusterings.length
    var k = 0
    while ( k < clusterings.length ) {
      val p = clusterings(k)
      var i = 0
      while ( i < n ) {
        val xi = x(i)
        val pi = p(i)
        var j = 0
        while ( j < n ) {
          if ( pi == p(j) ) xi(j) += unit
          j += 1
        }
        i += 1
      }
      k += 1
    }
    x
  }

  @tailrec
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

  def confidenceComputations[A](clustering: Array[Int], ppm: Array[Array[Double]]): (Array[Int], Array[Double], Array[Array[Double]], Array[Int], Array[Int]) = {
    val nItems = ppm.length
    assert(clustering.length == nItems)
    def overlap(subset1: Set[Int], subset2: Set[Int]): Double = {
      subset1.map(i => {
        val ppmi = ppm(i)
        subset2.map(j => {
          ppmi(j)
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
      val ppmi = ppm(i)
      val subset = partition.find(_.contains(i)).get
      confidence(i) = subset.map(j => ppmi(j)).sum / subset.size
    }
    val UNINITIALIZED = -1
    val exemplar = Array.fill(map.size) { UNINITIALIZED }
    val order = Array.range(0, nItems).sortWith((i, j) => {
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
    })
    val labels = Array.range(0, nItems).map(i => map(partition.find(_.contains(i)).get))
    (labels, confidence, matrixOutSmall, order, exemplar)
  }

}


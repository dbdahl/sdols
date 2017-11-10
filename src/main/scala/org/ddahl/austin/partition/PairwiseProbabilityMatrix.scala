package org.ddahl.austin.partition

object PairwiseProbabilityMatrix {

  // Assumes that 'clusterings' is a non-zero-length array of arrays of equal lengths.
  def apply(clusterings: Array[Array[Int]]): Array[Array[Double]] = {
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

  type Subset = Set[Int]

  def confidenceComputations[A](ppm: Array[Array[Double]], partition: Partition[A]): (Array[Int], Array[Double], Array[Array[Double]], Array[Int], Array[Int]) = {
    def overlap(subset1: Subset, subset2: Subset): Double = {
      subset1.map(i => subset2.map(j => ppm(i, j)).sum).sum / (subset1.size * subset2.size)
    }
    val matrix = scala.collection.mutable.HashMap[Tuple2[Subset, Subset], Double]()
    partition.map(s1 => partition.map(s2 => matrix((s1, s2)) = overlap(s1, s2)))
    val subsetsWithSumOverlap = partition.toList.map(s1 => (s1, partition.map(s2 => matrix(s1, s2)).sum))
    val sortedClusters = subsetsWithSumOverlap.sortWith((t1, t2) => t1._2 < t2._2).map(_._1)
    val map = sortedClusters.zipWithIndex.toMap
    val matrixOutSmall = Array.ofDim[Double](partition.nSubsets, partition.nSubsets)
    partition.map(c1 => partition.map(c2 => matrixOutSmall(map(c1))(map(c2)) = matrix((c1, c2))))
    val confidence = new Array[Double](nItems)
    for (i <- 0 until partition.nItems) {
      val subset = partition.subsetFor(i)
      confidence(i) = subset.map(j => apply(i, j)).sum / subset.nItems
    }
    val UNINITIALIZED = -1
    val exemplar = Array.fill(map.size)({ UNINITIALIZED })
    val order = Array.range(0, nItems).sortWith((i, j) => {
      val io = map(partition.subsetFor(i))
      val jo = map(partition.subsetFor(j))
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
    val labels = Array.range(0, nItems).map(i => map(partition.subsetFor(i)))
    (labels, confidence, matrixOutSmall, order, exemplar)
  }

}


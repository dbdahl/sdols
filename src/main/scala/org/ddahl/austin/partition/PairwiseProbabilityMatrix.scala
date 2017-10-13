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

}


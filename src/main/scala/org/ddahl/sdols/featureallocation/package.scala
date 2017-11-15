package org.ddahl.sdols

package object featureallocation {

   implicit val orderingVectorDouble = new Ordering[Vector[Double]]() {

    def compare(a: Vector[Double], b: Vector[Double]): Int = {
      if (a.size < b.size) -1
      else if (a.size > b.size) 1
      else {
        a.zip(b).foreach(x => {
          if (x._1 < x._2) return -1
          if (x._1 > x._2) return 1
        })
        0
      }
    }

  }

}

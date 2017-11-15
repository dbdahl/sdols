package org.ddahl.sdols.featureallocation

import scala.collection.mutable.ArrayBuffer

class FeatureAllocation[A <% Ordered[A]] private (val nItems: Int, val isLeftOrderedForm: Boolean, private val features: Vector[Feature[A]]) extends Iterable[Feature[A]] {

  override def canEqual(a: Any) = a.isInstanceOf[FeatureAllocation[A]]

  override def equals(that: Any): Boolean = that match {
    case that: FeatureAllocation[A] => that.canEqual(this) && this.leftOrderedForm.features == that.leftOrderedForm.features
    case _ => false
  }

  override def hashCode: Int = leftOrderedForm.features.hashCode

  def leftOrderedForm = if ( isLeftOrderedForm ) this else  _leftOrderedForm

  private lazy val _leftOrderedForm = new FeatureAllocation(nItems,true,features.sorted)

  def iterator: Iterator[Feature[A]] = features.iterator
  override def toVector: Vector[Feature[A]] = features
  override def toSeq: Vector[Feature[A]] = features
  override def toIndexedSeq: scala.collection.immutable.IndexedSeq[Feature[A]] = features
  override def toList: List[Feature[A]] = features.toList
  def toArray: Array[Feature[A]] = features.toArray
  override def size: Int = features.size

  lazy val isClusterAllocation: Boolean = {
    def check: Boolean = {
      val union = features.foldLeft(Set[Int]())((union, f) => {
        if (f.set.isEmpty) return false
        if (!union.intersect(f.set).isEmpty) return false
        union.union(f.set)
      })
      (union.min == 0) && (union.size == nItems)
    }
    check
  }

  def add(feature: Feature[A]) = {
    if (feature.size > 0) new FeatureAllocation(nItems,false,feature +: features) else this
  }

  def add(features: Iterable[Feature[A]]) = {
    new FeatureAllocation(nItems,false,features.filter(_.size > 0).toVector ++ this.features)
  }

  def add(i: Int, parameter: A) = {
    new FeatureAllocation(nItems,false,Feature(parameter, i) +: features)
  }

  def add(i: Int, feature: Feature[A]) = {
    val k = features.indexOf(feature)
    new FeatureAllocation(nItems, false, features.updated(k, features(k).add(i)))
  }

  def replace(oldFeature: Feature[A], newFeature: Feature[A]) = {
    val k = features.indexOf(oldFeature)
    new FeatureAllocation(nItems,false,features.updated(k, newFeature))
  }

  def remove(feature: Feature[A]) = {
    val (left, right) = features.span(_ != feature)
    if (right.isEmpty) throw new IllegalArgumentException("That feature is not in the allocation.")
    new FeatureAllocation(nItems,isLeftOrderedForm,left ++ right.tail)
  }

  def remove(features: Iterable[Feature[A]]) = {
    def engine(fRemaining: Iterable[Feature[A]], fRemoving: Iterable[Feature[A]]): Iterable[Feature[A]] = {
      if (fRemoving.isEmpty) return fRemaining
      val (left, right) = fRemaining.span(_ != fRemoving.head)
      if (right.isEmpty) throw new IllegalArgumentException("Some feature is not in the allocation.")
      left ++ engine(right.tail, fRemoving.tail)
    }
    new FeatureAllocation(nItems,isLeftOrderedForm,engine(this.features, features).toVector)
  }

  def remove(i: Int, feature: Feature[A]) = {
    val (left, right) = features.span(_ != feature)
    if (right.isEmpty) throw new IllegalArgumentException("That feature is not in the allocation.")
    val strippedFeature = right.head.remove(i)
    if (strippedFeature.size == 0) new FeatureAllocation(nItems,false,left ++ right.tail)
    else new FeatureAllocation(nItems,false,left ++ (strippedFeature +: right.tail))
  }

  def remove(i: Int) = {
    new FeatureAllocation(nItems,false,features.map(_.remove(i)).filterNot(_.size == 0))
  }

  def remove(i: Set[Int]) = {
    new FeatureAllocation(nItems,false,features.map(_.remove(i)).filterNot(_.size == 0))
  }

  def featuresOf(i: Int) = features.filter(_.contains(i))

  def coFeatured(i: Int, j: Int): Boolean = features.forall(f => f.contains(i) == f.contains(j))

  def nFeatures(i: Int) = features.count(_.contains(i))

  // Assumes the natural ordering.
  def nNewFeatures(i: Int) = featuresOf(i).filter(_.set.forall(_>=i)).size

  def nFeatures = features.size

  def rate = features.foldLeft(0)((sum, f) => sum + f.size).toDouble / (nItems * features.size)

  def dropParameters: FeatureAllocation[Null] = {
    new FeatureAllocation(nItems,isLeftOrderedForm,features.map(_.dropParameter))
  }

  def toMap = features.groupBy(identity).mapValues(_.size)

  def sharedFeaturesMatrix: Array[Array[Int]] = {
    val r = Array.ofDim[Int](nItems, nItems)
    features.foreach(f => {
      val indices = f.set.toList.filter(_ < nItems).sortWith(_ > _)
      var x = indices
      while (!x.isEmpty) {
        val rr = r(x.head)
        rr(x.head) += 1
        x.tail.foreach(rr(_) += 1)
        x = x.tail
      }
    })
    r
  }

  def coFeaturedIndicatorMatrix: Array[Array[Int]] = {
    val x = toMatrix(true)
    val r = Array.ofDim[Int](nItems, nItems)
    for (i <- 0 until nItems) {
      for (j <- 0 until i) {
        r(i)(j) = if (x(i).indices.filter(_ < nItems).forall(k => x(i)(k) == x(j)(k))) 1 else 0
      }
    }
    r
  }

  private def toMatrix[B: scala.reflect.ClassTag](value: B): Array[Array[B]] = {
    val Z = Array.ofDim[B](nItems, features.size)
    var k = 0
    features.foreach { f =>
      f.set.filter(_ < nItems).foreach { i => Z(i)(k) = value }
      k += 1
    }
    Z
  }

  def toMatrix: Array[Array[Int]] = toMatrix(1)

  def toAllocationString(zeroBased: Boolean) = {
    if (features.isEmpty) "{ }"
    else {
      "{ " + leftOrderedForm.features.map(_.toAllocationString(zeroBased)).mkString(", ") + " }"
    }
  }

  def toString(zeroBased: Boolean) = {
    if (features.isEmpty) "{ }"
    else {
      "{ " + leftOrderedForm.features.map(_.toString(zeroBased)).mkString(", ") + " }"
    }
  }

  override def toString = toString(true)

}

object FeatureAllocation {

  def apply[A: Ordering](nItems: Int, features: Vector[Feature[A]]): FeatureAllocation[A] = new FeatureAllocation(nItems,false,features.filter(_.size>0))

  def apply[A: Ordering](nItems: Int, features: Feature[A]*): FeatureAllocation[A] = new FeatureAllocation(nItems,false,features.filter(_.size>0).toVector)

  def empty[A: Ordering](nItems: Int) = new FeatureAllocation(nItems,true,Vector[Feature[A]]())

  def enumerate[A: Ordering](nItems: Int, atoms: List[A], maxNFeatures: Int): List[FeatureAllocation[A]] = {
    var list = List[FeatureAllocation[A]]()
    foreach(nItems, atoms, maxNFeatures) {f =>
      list = f +: list
    }
    list
  }

  def foreach[A: Ordering](nItems: Int, atoms: List[A], maxNFeatures: Int)(f: (FeatureAllocation[A]) => Unit): Unit = {
    def engine(base: FeatureAllocation[A], i: Int, availableFeatures: Seq[Feature[A]]): Unit = {
      if (i >= nItems) f(base)
      else if (!availableFeatures.isEmpty) {
        engine(base, i, availableFeatures.tail.filter(_ != availableFeatures.head))
        engine(base.add(i, availableFeatures.head), i, availableFeatures.tail)
      } else {
        def spider(current: FeatureAllocation[A], remainingSupport: List[A], nAllocated: Int, nTarget: Int): Unit = {
          if (nAllocated == nTarget) {
            engine(current, i + 1, current.features)
          } else if (!remainingSupport.isEmpty) {
            var pop = current
            spider(pop, remainingSupport.tail, nAllocated, nTarget)
            for (k <- 1 to nTarget - nAllocated) {
              pop = pop.add(i, remainingSupport.head)
              spider(pop, remainingSupport.tail, nAllocated + k, nTarget)
            }
          }
        }
        (0 to (maxNFeatures - base.nFeatures)).foreach(spider(base, atoms, 0, _))
      }
    }
    engine(empty[A](nItems), 0, empty[A](nItems).features)
  }

  def serialize[A](featureAllocations: Array[FeatureAllocation[A]]): Array[Int] = {
    if ( featureAllocations.isEmpty ) return Array[Int]()
    val fas = featureAllocations.map(_.dropParameters.leftOrderedForm)
    val data = ArrayBuffer[Int]()
    data += fas.head.nItems
    data += fas.size
    fas.foreach { fa =>
      data += fa.size
      fa.features.foreach { f =>
        data += f.size
        data ++= f.set.toArray
      }
    }
    data.toArray
  }

  def serializeWithParameters(featureAllocations: Array[FeatureAllocation[Vector[Double]]]): (Array[Int], Array[Double]) = {
    if ( featureAllocations.isEmpty ) return (Array[Int](),Array[Double]())
    val fas = featureAllocations.map(_.leftOrderedForm)
    val data = ArrayBuffer[Int]()
    val data2 = ArrayBuffer[Double]()
    var sizeDeclared = false
    data += fas.head.nItems
    data += fas.length
    fas.foreach { fa =>
      data += fa.size
      fa.features.foreach { f =>
        data += f.size
        data ++= f.set.toArray
        if ( ! sizeDeclared ) {
          sizeDeclared = true
          data2 += f.parameter.length
        }
        data2 ++= f.parameter
      }
    }
    (data.toArray, data2.toArray)
  }

  def deserialize(data: Array[Int]): Array[FeatureAllocation[Null]] = {
    if ( data.isEmpty ) return Array[FeatureAllocation[Null]]()
    val iter = data.iterator
    val nItems = iter.next()
    val N = iter.next()
    val fas = Array.ofDim[FeatureAllocation[Null]](N)
    for ( i <- 0 until N ) {
      var fa = FeatureAllocation.empty[Null](nItems)
      val K = iter.next()
      for ( k <- 0 until K ) {
        fa = fa.add(Feature(Seq.fill(iter.next()) { iter.next() }:_*))
      }
      fas(i) = fa
    }
    fas
  }

  def deserializeWithParameters(data: Array[Int], data2: Array[Double]): Array[FeatureAllocation[Vector[Double]]] = {
    if ( data.isEmpty ) return Array[FeatureAllocation[Vector[Double]]]()
    val iter = data.iterator
    val iter2 = data2.iterator
    var sizeDeclared = false
    var size = -1
    val nItems = iter.next()
    val N = iter.next()
    val fas = Array.ofDim[FeatureAllocation[Vector[Double]]](N)
    for ( i <- 0 until N ) {
      var fa = FeatureAllocation.empty[Vector[Double]](nItems)
      val K = iter.next()
      for ( k <- 0 until K ) {
        if ( ! sizeDeclared ) {
          sizeDeclared = true
          size = iter2.next().toInt
        }
        val parameter = Vector.fill(size) { iter2.next() }
        fa = fa.add(Feature(parameter,Seq.fill(iter.next()) { iter.next() }:_*))
      }
      fas(i) = fa
    }
    fas
  }

}

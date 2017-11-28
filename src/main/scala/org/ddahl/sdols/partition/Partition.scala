package org.ddahl.sdols
package partition

import scala.reflect.ClassTag

final class Partition[A](val nItems: Int, val nSubsets: Int, protected val x: Set[Subset[A]]) extends Iterable[Subset[A]] {

  private val checks = false

  if (checks) {
    if (nSubsets != x.size) throw new RuntimeException("Internal error")
    if (x.foldLeft(0)((sum, subset) => sum + subset.size) != nItems) throw new RuntimeException("Internal error")
  }

  def add(subset: Subset[A]): Partition[A] = {
    if (checks) {
      if (subset.size == 0) throw new IllegalArgumentException("Subset is empty.")
      if (contains(subset)) throw new IllegalArgumentException("Partition already contains this subset.")
    }
    new Partition(nItems + subset.size, nSubsets + 1, x + subset)
  }

  def add(i: Int, subset: Subset[A]): Partition[A] = {
    if (checks) {
      if (contains(i)) throw new IllegalArgumentException("Partition already contains " + i + ".")
    }
    val newSubset = subset.add(i)
    if (contains(subset)) new Partition(nItems + 1, nSubsets, x - subset + newSubset)
    else new Partition(nItems + 1, nSubsets + 1, x + newSubset)
  }

  def remove(subset: Subset[A]): Partition[A] = {
    if (checks) {
      if (!contains(subset)) throw new IllegalArgumentException("Partition does not contain this subset.")
    }
    new Partition(nItems - subset.size, nSubsets - 1, x - subset)
  }

  def remove(i: Int, subset: Subset[A]): Partition[A] = {
    if (checks) {
      if (!subset.contains(i)) throw new IllegalArgumentException("Subset does not contain " + i + ".")
    }
    if (subset.size == 1) new Partition(nItems - 1, nSubsets - 1, x - subset)
    else new Partition(nItems - 1, nSubsets, x - subset + subset.remove(i))
  }

  def remove(i: Int): Partition[A] = {
    val s = x.find(_.contains(i))
    if (checks) {
      if (s.isEmpty) throw new IllegalArgumentException("Partition does not contain " + i + ".")
    }
    val subset = s.get
    remove(i, subset)
  }

  def removeWithSubset(i: Int): (Partition[A], Subset[A]) = {
    val s = x.find(_.contains(i))
    if (checks) {
      if (s.isEmpty) throw new IllegalArgumentException("Partition does not contain " + i + ".")
    }
    val subset = s.get
    val subsetWithoutI = subset.remove(i)
    if (subset.size == 1) {
      (new Partition(nItems - 1, nSubsets - 1, x - subset), subsetWithoutI)
    } else {
      (new Partition(nItems - 1, nSubsets, x - subset + subsetWithoutI), subsetWithoutI)
    }
  }

  def replace(func: (Subset[A]) => A): Partition[A] = {
    Partition(map(subset => {
      subset.replace(func(subset))
    }))
  }

  def contains(subset: Subset[A]): Boolean = x.contains(subset)

  def contains(i: Int): Boolean = x.exists(_.contains(i))

  def paired(i: Int, k: Int): Boolean = {
    val subset = x.find(y => y.contains(i) || y.contains(k)).get
    subset.contains(i) && subset.contains(k)
  }

  def subsetFor(i: Int): Subset[A] = {
    x.find(_.contains(i)).get
  }

  def parameterFor(i: Int): A = {
    subsetFor(i).parameter
  }

  def pairwiseAllocationMatrix: Array[Array[Int]] = {
    val r = Array.ofDim[Int](nItems, nItems)
    x.foreach(s => {
      val indices = s.toList.filter(_ < nItems).sortWith(_ > _)
      var x = indices
      while (!x.isEmpty) {
        val rr = r(x.head)
        rr(x.head) += 1
        x.tail.foreach(rr(_) += 1)
        x = x.tail
      }
    })
    // Symmetrize
    var i = 0
    while ( i < nItems ) {
      val ri = r(i)
      var j = i + 1
      while ( j < nItems ) {
        ri(j) = r(j)(i)
        j += 1
      }
      i += 1
    }
    r
  }

  override def equals(other: Any): Boolean = other match {
    case that: Partition[A] =>
      if (that.nItems != nItems) false
      else that.x == x
    case _ => false
  }

  override def hashCode: Int = x.hashCode

  def iterator = x.iterator

  override def toString: String = "{" + map(_.toString).toList.sortWith(_ < _).mkString(",") + "}"

  def toStringTerse: String = "{" + map(_.toStringTerse).toList.sortWith(_ < _).mkString(",") + "}"

  def entropy: Double = {
    val rates = map(_.size.asInstanceOf[Double] / nItems).toList.sortWith(_ > _)
    rates.foldLeft(0.0)((s, p) => { s - (if (p > 0.0) p * math.log(p) else 0.0) })
  }

  def toLabels: Array[Int] = {
    val result = new Array[Int](nItems)
    var label = 0
    toList.sortWith(_.min < _.min).foreach(subset => {
      subset.foreach(i => result(i) = label)
      label += 1
    })
    result
  }

  def toLabelsWithParameters(implicit m: ClassTag[A]): (Array[Int], Array[A]) = {
    val result = new Array[Int](nItems)
    val resultParameters = new Array[A](nSubsets)
    var label = 0
    toList.sortWith(_.min < _.min).foreach(subset => {
      subset.foreach(i => result(i) = label)
      resultParameters(label) = subset.parameter
      label += 1
    })
    (result, resultParameters)
  }

  def write(objOutputStream: java.io.ObjectOutputStream): Unit = {
    objOutputStream.writeInt(nSubsets)
    iterator.foreach(_.write(objOutputStream))
  }

}

object Partition {

  def empty[A](): Partition[A] = new Partition(0, 0, Set[Subset[A]]())

  def apply[A](sampler: () => A, nItems: Int, allTogether: Boolean): Partition[A] = {
    if (allTogether) {
      Partition(Subset(sampler(), Range(0, nItems)))
    } else {
      Partition(Range(0, nItems).map(i => Subset(sampler(), i)): _*)
    }
  }

  import scala.annotation.tailrec

  @tailrec private def makePartition[A](sampler: () => A, labelsWithIndex: Iterable[(Int, Int)], list: List[Subset[A]]): List[Subset[A]] = {
    val label = labelsWithIndex.head._1
    val (left, right) = labelsWithIndex.partition(_._1 == label)
    val longerList = Subset(sampler(), left.map(_._2)) +: list
    if (right.isEmpty) longerList
    else makePartition(sampler, right, longerList)
  }

  def apply[A](sampler: () => A, labels: Iterable[Int]): Partition[A] = {
    if (labels.isEmpty) throw new IllegalArgumentException("Labels may not by empty.")
    apply(makePartition(sampler, labels.zipWithIndex, List[Subset[A]]()))
  }

  def apply(labels: Array[Int]): Partition[Null] = apply(() => null, labels)

  def apply[A](i: Subset[A]*): Partition[A] = apply(i)

  def apply[A](i: Iterable[Subset[A]]): Partition[A] = {
    val nItems = i.foldLeft(0)((sum, subset) => sum + subset.size)
    new Partition(nItems, i.size, i.toSet)
  }

  def apply[A](objInputStream: java.io.ObjectInputStream): Partition[A] = {
    val nSubsets = objInputStream.readInt()
    val seq = Seq.fill(nSubsets) { Subset[A](objInputStream) }
    apply(seq)
  }

  def enumerate[A](sampler: () => A, nItems: Int): List[Partition[A]] = {
    var cache = List[Partition[A]]()
    def engine(partition: Partition[A], nextItem: Int, nItems: Int): Unit = {
      if (nextItem == nItems) {
        cache = partition +: cache
      } else {
        partition.foreach(subset => {
          engine(partition.add(nextItem, subset), nextItem + 1, nItems)
        })
        engine(partition.add(nextItem, Subset.empty(sampler())), nextItem + 1, nItems)
      }
    }
    engine(empty[A](), 0, nItems)
    cache
  }

}

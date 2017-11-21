package org.ddahl.sdols
package partition

import scala.reflect.ClassTag

trait Partition[A] extends Iterable[Subset[A]] {

  val nItems: Int

  val nSubsets: Int

  def add(subset: Subset[A]): Partition[A]

  def add(i: Int, subset: Subset[A]): Partition[A]

  def remove(subset: Subset[A]): Partition[A]

  def remove(i: Int, subset: Subset[A]): Partition[A]

  def remove(i: Int): Partition[A]

  def removeWithSubset(i: Int): (Partition[A], Subset[A])

  def replaceParameters(func: (Subset[A]) => A): Partition[A]

  def contains(subset: Subset[A]): Boolean

  def contains(i: Int): Boolean

  def paired(i: Int, k: Int): Boolean

  def subsetFor(i: Int): Subset[A]

  def parameterFor(i: Int): A

  override def toString = "{" + map(_.toString).toList.sortWith(_ < _).mkString(",") + "}"

  def toStringTerse = "{" + map(_.toStringTerse).toList.sortWith(_ < _).mkString(",") + "}"

  def entropy: Double = {
    val rates = map(_.nItems.asInstanceOf[Double] / nItems).toList.sortWith(_ > _)
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

  def write(objOutputStream: java.io.ObjectOutputStream) = {
    objOutputStream.writeInt(nSubsets)
    iterator.foreach(_.write(objOutputStream))
  }

}

final class SetPartition[A](val nItems: Int, val nSubsets: Int, protected val x: Set[Subset[A]]) extends Partition[A] {

  private val checks = false

  if (checks) {
    if (nSubsets != x.size) throw new RuntimeException("Internal error")
    if (x.foldLeft(0)((sum, subset) => sum + subset.nItems) != nItems) throw new RuntimeException("Internal error")
  }

  def add(subset: Subset[A]) = {
    if (checks) {
      if (subset.nItems == 0) throw new IllegalArgumentException("Subset is empty.")
      if (contains(subset)) throw new IllegalArgumentException("Partition already contains this subset.")
    }
    new SetPartition(nItems + subset.nItems, nSubsets + 1, x + subset)
  }

  def add(i: Int, subset: Subset[A]) = {
    if (checks) {
      if (contains(i)) throw new IllegalArgumentException("Partition already contains " + i + ".")
    }
    val newSubset = subset.add(i)
    if (contains(subset)) new SetPartition(nItems + 1, nSubsets, x - subset + newSubset)
    else new SetPartition(nItems + 1, nSubsets + 1, x + newSubset)
  }

  def remove(subset: Subset[A]) = {
    if (checks) {
      if (!contains(subset)) throw new IllegalArgumentException("Partition does not contain this subset.")
    }
    new SetPartition(nItems - subset.nItems, nSubsets - 1, x - subset)
  }

  def remove(i: Int, subset: Subset[A]) = {
    if (checks) {
      if (!subset.contains(i)) throw new IllegalArgumentException("Subset does not contain " + i + ".")
    }
    if (subset.nItems == 1) new SetPartition(nItems - 1, nSubsets - 1, x - subset)
    else new SetPartition(nItems - 1, nSubsets, x - subset + subset.remove(i))
  }

  def remove(i: Int) = {
    val s = x.find(_.contains(i))
    if (checks) {
      if (s.isEmpty) throw new IllegalArgumentException("Partition does not contain " + i + ".")
    }
    val subset = s.get
    remove(i, subset)
  }

  def removeWithSubset(i: Int) = {
    val s = x.find(_.contains(i))
    if (checks) {
      if (s.isEmpty) throw new IllegalArgumentException("Partition does not contain " + i + ".")
    }
    val subset = s.get
    val subsetWithoutI = subset.remove(i)
    if (subset.nItems == 1) {
      (new SetPartition(nItems - 1, nSubsets - 1, x - subset), subsetWithoutI)
    } else {
      (new SetPartition(nItems - 1, nSubsets, x - subset + subsetWithoutI), subsetWithoutI)
    }
  }

  def replaceParameters(func: (Subset[A]) => A) = {
    Partition(map(subset => {
      val newParameter = func(subset)
      subset.replaceParameter(newParameter)
    }))
  }

  def contains(subset: Subset[A]) = x.contains(subset)

  def contains(i: Int) = x.exists(_.contains(i))

  def paired(i: Int, k: Int) = {
    val subset = x.find(y => y.contains(i) || y.contains(k)).get
    subset.contains(i) && subset.contains(k)
  }

  def subsetFor(i: Int): Subset[A] = {
    x.find(_.contains(i)).get
  }

  def parameterFor(i: Int): A = {
    subsetFor(i).parameter
  }

  override def equals(other: Any) = other match {
    case that: SetPartition[A] =>
      if (that.nItems != nItems) false
      else that.x == x
    case _ => false
  }

  override def hashCode: Int = x.hashCode

  def iterator = x.iterator

}

object Partition {

  def empty[A](): Partition[A] = new SetPartition(0, 0, Set[Subset[A]]())

  def apply[A](sampler: () => A, nItems: Int, allTogether: Boolean): Partition[A] = {
    if (allTogether) {
      Partition(Subset(sampler, Range(0, nItems)))
    } else {
      Partition(Range(0, nItems).map(i => Subset(sampler, i)): _*)
    }
  }

  import scala.annotation.tailrec

  @tailrec private def makePartition[A](sampler: () => A, labelsWithIndex: Iterable[(Int, Int)], list: List[Subset[A]]): List[Subset[A]] = {
    val label = labelsWithIndex.head._1
    val (left, right) = labelsWithIndex.partition(_._1 == label)
    val longerList = Subset(sampler, left.map(_._2)) +: list
    if (right.isEmpty) longerList
    else makePartition(sampler, right, longerList)
  }

  def apply[A](sampler: () => A, labels: Iterable[Int]): Partition[A] = {
    if (labels.isEmpty) throw new IllegalArgumentException("Labels may not by empty.")
    apply(makePartition(sampler, labels.zipWithIndex, List[Subset[A]]()))
  }

  def apply[A](i: Subset[A]*): Partition[A] = apply(i)

  def apply[A](i: Iterable[Subset[A]]): Partition[A] = {
    val nItems = i.foldLeft(0)((sum, subset) => sum + subset.nItems)
    new SetPartition(nItems, i.size, i.toSet)
  }

  def apply[A](sampler: () => A, objInputStream: java.io.ObjectInputStream): Partition[A] = {
    val nSubsets = objInputStream.readInt()
    val seq = Seq.fill(nSubsets) { Subset(sampler,objInputStream) }
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
        engine(partition.add(nextItem, Subset.empty(sampler)), nextItem + 1, nItems)
      }
    }
    engine(empty[A](), 0, nItems)
    cache
  }

}


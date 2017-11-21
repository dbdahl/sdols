package org.ddahl.sdols
package partition

trait Subset[A] extends Iterable[Int] {

  val parameter: A

  val nItems: Int

  protected[partition] def add(i: Int): Subset[A]

  protected[partition] def add(subset: Int*): Subset[A]

  protected[partition] def remove(i: Int): Subset[A]

  protected[partition] def remove(subset: Int*): Subset[A]

  protected[partition] def replaceParameter(newParameter: A): Subset[A]

  def contains(i: Int): Boolean

  def contains(subset: Int*): Boolean

  override def toString = {
    "({" + toList.sortWith(_ < _).mkString(",") + "}," + parameter + ")"
  }

  def toStringTerse = {
    "{" + toList.sortWith(_ < _).mkString(",") + "}"
  }
  
  def write(objOutputStream: java.io.ObjectOutputStream) = {
    objOutputStream.writeInt(nItems)
    iterator.foreach(objOutputStream.writeInt)
    objOutputStream.writeObject(parameter)
  }

}

final class SetSubset[A] private[partition] (val nItems: Int, val x: Set[Int], val parameter: A, samplingModel: SamplingModel2[A]) extends Subset[A] {

  private val checks = false

  if (checks) {
    if (nItems != x.size) throw new RuntimeException("Internal error")
  }

  protected[partition] def add(i: Int) = {
    if (checks) {
      if (contains(i)) throw new IllegalArgumentException("Subset already contains " + i + ".")
    }
    new SetSubset(nItems + 1, x + i, samplingModel.copy(parameter), samplingModel)
  }

  protected[partition] def add(subset: Int*) = {
    if (checks) {
      if (subset.exists(x.contains)) throw new IllegalArgumentException("Subset already contains at least one of these items.")
    }
    new SetSubset(nItems + subset.size, x ++ subset, samplingModel.copy(parameter), samplingModel)
  }

  protected[partition] def remove(i: Int) = {
    if (checks) {
      if (!contains(i)) throw new IllegalArgumentException("Subset does not contain " + i + ".")
    }
    new SetSubset(nItems - 1, x - i, samplingModel.copy(parameter), samplingModel)
  }

  protected[partition] def remove(subset: Int*) = {
    if (checks) {
      if (!subset.forall(x.contains)) throw new IllegalArgumentException("Subset does not contain all of these items.")
    }
    new SetSubset(nItems - subset.size, x -- subset, samplingModel.copy(parameter), samplingModel)
  }

  protected[partition] def replaceParameter(newParameter: A) = {
    new SetSubset(nItems, x, newParameter, samplingModel)
  }

  def contains(i: Int) = x.contains(i)

  def contains(subset: Int*) = subset.forall(x.contains)

  override def equals(other: Any) = other match {
    case that: SetSubset[A] =>
      if (that.nItems != nItems) false
      else that.x == x
    case _ => false
  }

  override def hashCode: Int = x.hashCode

  def iterator = x.iterator

}

object Subset {

  def empty[A](samplingModel: SamplingModel2[A]): Subset[A] = new SetSubset(0, Set[Int](), samplingModel.sample, samplingModel)

  def apply[A](samplingModel: SamplingModel2[A], i: Int): Subset[A] = new SetSubset(1, Set[Int](i), samplingModel.sample, samplingModel)

  def apply[A](samplingModel: SamplingModel2[A], i: Iterable[Int]): Subset[A] = new SetSubset(i.size, i.toSet, samplingModel.sample, samplingModel)

  def apply[A](samplingModel: SamplingModel2[A], i: Int*): Subset[A] = apply(samplingModel, i)

  def apply[A](samplingModel: SamplingModel2[A], objInputStream: java.io.ObjectInputStream): Subset[A] = {
    val nItems = objInputStream.readInt()
    val seq = Seq.fill(nItems) { objInputStream.readInt() }
    val parameter = objInputStream.readObject().asInstanceOf[A]
    new SetSubset(nItems,seq.toSet,parameter,samplingModel)
  }
  
}


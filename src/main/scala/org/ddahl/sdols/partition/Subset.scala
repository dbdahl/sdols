package org.ddahl.sdols
package partition

trait Subset[A] extends Iterable[Int] {

  val parameter: A

  protected[partition] def add(i: Int): Subset[A]

  protected[partition] def add(subset: Int*): Subset[A]

  protected[partition] def remove(i: Int): Subset[A]

  protected[partition] def remove(subset: Int*): Subset[A]

  protected[partition] def replace(parameter: A): Subset[A]

  def contains(i: Int): Boolean

  def contains(subset: Int*): Boolean

  override def toString = {
    "({" + toList.sortWith(_ < _).mkString(",") + "}," + parameter + ")"
  }

  def toStringTerse = {
    "{" + toList.sortWith(_ < _).mkString(",") + "}"
  }

  def write(objOutputStream: java.io.ObjectOutputStream) = {
    objOutputStream.writeInt(size)
    iterator.foreach(objOutputStream.writeInt)
    objOutputStream.writeObject(parameter)
  }

}

final class SetSubset[A] private[partition] (override val size: Int, val x: Set[Int], val parameter: A, sampler: () => A) extends Subset[A] {

  private val checks = false

  if (checks) {
    if (size != x.size) throw new RuntimeException("Internal error")
  }

  protected[partition] def add(i: Int) = {
    if (checks) {
      if (contains(i)) throw new IllegalArgumentException("Subset already contains " + i + ".")
    }
    new SetSubset(size + 1, x + i, parameter, sampler)
  }

  protected[partition] def add(subset: Int*) = {
    if (checks) {
      if (subset.exists(x.contains)) throw new IllegalArgumentException("Subset already contains at least one of these items.")
    }
    new SetSubset(size + subset.size, x ++ subset, parameter, sampler)
  }

  protected[partition] def remove(i: Int) = {
    if (checks) {
      if (!contains(i)) throw new IllegalArgumentException("Subset does not contain " + i + ".")
    }
    new SetSubset(size - 1, x - i, parameter, sampler)
  }

  protected[partition] def remove(subset: Int*) = {
    if (checks) {
      if (!subset.forall(x.contains)) throw new IllegalArgumentException("Subset does not contain all of these items.")
    }
    new SetSubset(size - subset.size, x -- subset, parameter, sampler)
  }

  protected[partition] def replace(parameter: A) = {
    new SetSubset(size, x, parameter, sampler)
  }

  def contains(i: Int) = x.contains(i)

  def contains(subset: Int*) = subset.forall(x.contains)

  override def equals(other: Any) = other match {
    case that: SetSubset[A] =>
      if (that.size != size) false
      else that.x == x
    case _ => false
  }

  override def hashCode: Int = x.hashCode

  def iterator = x.iterator

}

object Subset {

  def empty[A](sampler: () => A): Subset[A] = new SetSubset(0, Set[Int](), sampler(), sampler)

  def apply[A](sampler: () => A, i: Int): Subset[A] = new SetSubset(1, Set[Int](i), sampler(), sampler)

  def apply[A](sampler: () => A, i: Iterable[Int]): Subset[A] = new SetSubset(i.size, i.toSet, sampler(), sampler)

  def apply[A](sampler: () => A, i: Set[Int]): Subset[A] = new SetSubset(i.size, i, sampler(), sampler)

  def apply[A](sampler: () => A, i: Int*): Subset[A] = apply(sampler, i)

  def apply[A](sampler: () => A, objInputStream: java.io.ObjectInputStream): Subset[A] = {
    val nItems = objInputStream.readInt()
    val seq = Seq.fill(nItems) { objInputStream.readInt() }
    val parameter = objInputStream.readObject().asInstanceOf[A]
    new SetSubset(nItems,seq.toSet,parameter,sampler)
  }
  
}


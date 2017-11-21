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

final class SetSubset[A] private[partition] (override val size: Int, val x: Set[Int], val parameter: A) extends Subset[A] {

  private val checks = false

  if (checks) {
    if (size != x.size) throw new RuntimeException("Internal error")
  }

  protected[partition] def add(i: Int) = {
    if (checks) {
      if (contains(i)) throw new IllegalArgumentException("Subset already contains " + i + ".")
    }
    new SetSubset(size + 1, x + i, parameter)
  }

  protected[partition] def add(subset: Int*) = {
    if (checks) {
      if (subset.exists(x.contains)) throw new IllegalArgumentException("Subset already contains at least one of these items.")
    }
    new SetSubset(size + subset.size, x ++ subset, parameter)
  }

  protected[partition] def remove(i: Int) = {
    if (checks) {
      if (!contains(i)) throw new IllegalArgumentException("Subset does not contain " + i + ".")
    }
    new SetSubset(size - 1, x - i, parameter)
  }

  protected[partition] def remove(subset: Int*) = {
    if (checks) {
      if (!subset.forall(x.contains)) throw new IllegalArgumentException("Subset does not contain all of these items.")
    }
    new SetSubset(size - subset.size, x -- subset, parameter)
  }

  protected[partition] def replace(parameter: A) = {
    new SetSubset(size, x, parameter)
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

  def empty[A](parameter: A): Subset[A] = new SetSubset(0, Set[Int](), parameter)

  def apply[A](parameter: A, i: Int): Subset[A] = new SetSubset(1, Set[Int](i), parameter)

  def apply[A](parameter: A, i: Iterable[Int]): Subset[A] = new SetSubset(i.size, i.toSet, parameter)

  def apply[A](parameter: A, i: Set[Int]): Subset[A] = new SetSubset(i.size, i, parameter)

  def apply[A](parameter: A, i: Int*): Subset[A] = apply(parameter, i)

  def apply[A](objInputStream: java.io.ObjectInputStream): Subset[A] = {
    val nItems = objInputStream.readInt()
    val seq = Seq.fill(nItems) { objInputStream.readInt() }
    val parameter = objInputStream.readObject().asInstanceOf[A]
    new SetSubset(nItems,seq.toSet,parameter)
  }
  
}


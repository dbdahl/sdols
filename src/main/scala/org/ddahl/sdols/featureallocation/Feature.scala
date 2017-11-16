package org.ddahl.sdols.featureallocation

case class Feature[A] private (parameter: A, set: Set[Int], size: Int) extends Ordered[Feature[A]] {

  def compare(that: Feature[A]): Int = {
    val thisMax = this.set.max
    val thatMax = that.set.max
    val max = thisMax min thatMax
    var i = 0
    while ( i <= max ) {
      val a = this.set.contains(i)
      val b = that.set.contains(i)
      if ( a != b ) return if ( a ) -1 else 1
      i += 1
    }
    if ( thisMax > thatMax ) -1
    else if ( thisMax < thatMax ) 1
    else if ((this.parameter == null) && (that.parameter == null)) 0
    else if (this.parameter == null) 1
    else if (that.parameter == null) -1
    else 0
  }

  def dropParameter = Feature(null: Null, set, size)

  def replace(parameter: A) = Feature(parameter, set, size)

  def add(i: Int) = {
    if ((i < 0) || set.contains(i)) this
    else Feature(parameter, set + i, size + 1)
  }

  def remove(i: Int) = {
    if (set.contains(i)) Feature(parameter, set - i, size - 1)
    else this
  }

  def remove(i: Set[Int]) = {
    val diff = set.diff(i)
    Feature(parameter, diff, diff.size)
  }

  def intersect(i: Set[Int]) = {
    val intersect = set.intersect(i)
    Feature(parameter, intersect, intersect.size)
  }

  def contains(i: Int) = set.contains(i)

  def subsetOf(f: Feature[A]) = {
    if (f.parameter != parameter) false
    else set.subsetOf(f.set)
  }

  def iterator = set.iterator

  def toAllocationString(zeroBased: Boolean) = {
    val array = set.toArray
    java.util.Arrays.sort(array)  // Do in-place sorting
    if ( ! zeroBased ) {          // Do in-place increment
      var i = 0
      while ( i < array.length ) {
        array(i) += 1
        i += 1
      }
    }
    "{" + array.mkString(",") + "}"
  }

  def toString(zeroBased: Boolean) = toAllocationString(zeroBased) + (if (parameter != null) " => " + parameter else "")

  override def toString = toString(true)

}

object Feature {

  def apply[A](parameter: A, items: Int*): Feature[A] = {
    val set = items.filter(_>=0).toSet
    Feature(parameter, set, set.size)
  }

  def apply[A](parameter: A, items: Array[Int]): Feature[A] = {
    val set = items.filter(_>=0).toSet
    Feature(parameter, set, set.size)
  }

  def apply[A](parameter: A, items: Set[Int]): Feature[A] = {
    val set = items.filter(_>=0)
    Feature(parameter, set, set.size)
  }

  def empty[A](parameter: A): Feature[A] = Feature(parameter, Set[Int](), 0)

  def apply(items: Int*): Feature[Null] = {
    val set = Set(items: _*).filter(_>=0)
    Feature(null: Null, set, set.size)
  }

  def empty: Feature[Null] = Feature(null: Null, Set[Int](), 0)

}

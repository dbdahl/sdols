package org.ddahl.austin

// Node

case class Node(index: Int)

// Edge

sealed abstract class Edge(val a: Node, val b: Node)

case class DirectedEdge(override val a: Node, override val b: Node) extends Edge(a, b)

case class UndirectedEdge(override val a: Node, override val b: Node) extends Edge(a, b) {

  override def equals(o: Any) = o match {
    case that: UndirectedEdge => that.a == a && that.b == b || that.a == b && that.b == a
    case _ => false
  }

  override def hashCode = a.hashCode * b.hashCode // commutative and unique

}

// Graph

sealed abstract class Graph[A <: Edge](v: Set[Node], e: Set[A], directed: Boolean) {

  lazy val toAdjacencyMatrix = {
    val n = v.size
    val adjacency = Array.ofDim[Boolean](n, n)
    e.foreach(edge => {
      adjacency(edge.a.index)(edge.b.index) = true
      if ( ! directed ) adjacency(edge.b.index)(edge.a.index) = true
    })
    adjacency
  }

}

class UndirectedGraph private(val v: Set[Node], val e: Set[UndirectedEdge]) extends Graph(v, e, false)

object UndirectedGraph {

  def apply(v: Set[Node], e: Set[UndirectedEdge]): UndirectedGraph = {
    val allVinE = e.map(x => List(x.a, x.b)).flatten
    if (!allVinE.subsetOf(v)) throw new IllegalArgumentException("Arguments do not form a valid graph.")
    new UndirectedGraph(v, e)
  }

  def apply(adjacency: Array[Array[Boolean]]): UndirectedGraph = {
    val vertices = adjacency.indices.map(i => Node(i)).toSet
    val edges = scala.collection.mutable.Set[UndirectedEdge]()
    for (i <- adjacency.indices) {
      for (j <- 0 until i) {
        if (adjacency(i)(j)) edges += UndirectedEdge(Node(i), Node(j))
      }
    }
    new UndirectedGraph(vertices, edges.toSet)
  }

}

class DirectedGraph[X] private(val v: Set[Node], val e: Set[DirectedEdge]) extends Graph(v, e, true)

object DirectedGraph {

  def apply[X](v: Set[Node], e: Set[DirectedEdge]): DirectedGraph[X] = {
    val allVinE = e.map(x => List(x.a, x.b)).flatten
    if (!allVinE.subsetOf(v)) throw new IllegalArgumentException("Arguments do not form a valid graph.")
    new DirectedGraph(v, e)
  }

  def apply(adjacency: Array[Array[Boolean]]): DirectedGraph[Int] = {
    val vertices = adjacency.indices.map(i => Node(i)).toSet
    val edges = scala.collection.mutable.Set[DirectedEdge]()
    for (i <- adjacency.indices) {
      for (j <- adjacency.indices) {
        if (adjacency(i)(j)) edges += DirectedEdge(Node(i), Node(j))
      }
    }
    new DirectedGraph(vertices, edges.toSet)
  }

}


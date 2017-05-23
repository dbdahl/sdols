package org.ddahl.austin

import java.io.{FileWriter, BufferedWriter, PrintWriter}
import org.apache.commons.math3.random.RandomGenerator
import org.apache.commons.math3.util.FastMath.exp

// Node

case class Node(index: Int)

// Edge

sealed abstract class Edge(val a: Node, val b: Node)

case class DirectedEdge(override val a: Node, override val b: Node) extends Edge(a, b)

case class UndirectedEdge(override val a: Node, override val b: Node) extends Edge(a, b) {

  override def equals(o: Any): Boolean = o match {
    case that: UndirectedEdge => that.a == a && that.b == b || that.a == b && that.b == a
    case _ => false
  }

  override def hashCode: Int = a.hashCode * b.hashCode // commutative and unique

}

// Graph

sealed abstract class Graph[A <: Edge](v: Set[Node], e: Set[A], directed: Boolean) {

  lazy val toAdjacencyMatrix: Array[Array[Boolean]] = {
    val n = v.size
    val adjacency = Array.ofDim[Boolean](n, n)
    e.foreach(edge => {
      adjacency(edge.a.index)(edge.b.index) = true
      if (!directed) adjacency(edge.b.index)(edge.a.index) = true
    })
    adjacency
  }

  def writeEdgeList(filename: String, separator: String = " "): Unit = {
    val writer = new PrintWriter(new BufferedWriter(new FileWriter(filename)))
    writer.println("Source"+separator+"Target")
    e.foreach(edge => {
      val tuple = if ( directed ) (edge.a,edge.b)
      else {
        if ( edge.a.index < edge.b.index ) (edge.a,edge.b)
        else (edge.b,edge.a)
      }
      writer.println(tuple._1.index + separator + tuple._2.index)
    })
    writer.close()
  }

}

class UndirectedGraph private(val v: Set[Node], val e: Set[UndirectedEdge]) extends Graph(v, e, false)

object UndirectedGraph {

  def apply(v: Set[Node], e: Set[UndirectedEdge]): UndirectedGraph = {
    val allVinE = e.flatMap(x => List(x.a, x.b))
    if (!allVinE.subsetOf(v)) throw new IllegalArgumentException("Arguments do not form a valid graph.")
    new UndirectedGraph(v, e)
  }

  def apply(adjacency: Array[Array[Boolean]]): UndirectedGraph = {
    val vertices = adjacency.indices.map(Node).toSet
    val edges = scala.collection.mutable.Set[UndirectedEdge]()
    for (i <- adjacency.indices) {
      for (j <- 0 until i) {
        if (adjacency(i)(j)) edges += UndirectedEdge(Node(i), Node(j))
      }
    }
    new UndirectedGraph(vertices, edges.toSet)
  }

  def sample(weights: Array[Double], rng: RandomGenerator): UndirectedGraph = {
    def probLink(w1: Double, w2: Double) = 1 - exp(-2*w1*w2)
    val vertices = weights.indices.map(Node).toSet
    val edges = scala.collection.mutable.Set[UndirectedEdge]()
    for ( i <- weights.indices ) {
      for ( j <- 0 until i ) {
        if ( ( i != j ) && ( rng.nextDouble() < probLink(weights(i),weights(j)) ) ) edges += UndirectedEdge(Node(i),Node(j))
      }
    }
    new UndirectedGraph(vertices,edges.toSet)
  }

}

class DirectedGraph private(val v: Set[Node], val e: Set[DirectedEdge]) extends Graph(v, e, true)

object DirectedGraph {

  def apply(v: Set[Node], e: Set[DirectedEdge]): DirectedGraph = {
    val allVinE = e.flatMap(x => List(x.a, x.b))
    if (!allVinE.subsetOf(v)) throw new IllegalArgumentException("Arguments do not form a valid graph.")
    new DirectedGraph(v, e)
  }

  def apply(adjacency: Array[Array[Boolean]]): DirectedGraph = {
    val vertices = adjacency.indices.map(Node).toSet
    val edges = scala.collection.mutable.Set[DirectedEdge]()
    for (i <- adjacency.indices) {
      for (j <- adjacency.indices) {
        if (adjacency(i)(j)) edges += DirectedEdge(Node(i), Node(j))
      }
    }
    new DirectedGraph(vertices, edges.toSet)
  }

  def sample(weights: Array[Double], rng: RandomGenerator): DirectedGraph = {
    def probLink(w1: Double, w2: Double) = 1 - exp(-2*w1*w2)
    val vertices = weights.indices.map(Node).toSet
    val edges = scala.collection.mutable.Set[DirectedEdge]()
    for ( i <- weights.indices ) {
      for ( j <- weights.indices ) {
        if ( ( i != j ) && ( rng.nextDouble() < probLink(weights(i),weights(j)) ) ) edges += DirectedEdge(Node(i),Node(j))
      }
    }
    new DirectedGraph(vertices,edges.toSet)
  }

}


package fr.clementgarbay.graph

/**
  * @author Clément Garbay
  */

trait Node {
  val id: Int
}
case class NodeDirected(id: Int, successors: Set[Int]) extends Node
case class NodeUndirected(id: Int, neighbors: Set[Int]) extends Node

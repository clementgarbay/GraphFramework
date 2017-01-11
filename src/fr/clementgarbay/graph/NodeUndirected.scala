package fr.clementgarbay.graph

/**
  * @author Clément Garbay
  */

trait Node {
  val id: Int
}
case class NodeDirected(id: Int, successors: List[NodeDirected]) extends Node
case class NodeUndirected(id: Int, neighbours: List[NodeUndirected]) extends Node

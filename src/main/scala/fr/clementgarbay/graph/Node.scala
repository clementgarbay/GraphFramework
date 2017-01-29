package fr.clementgarbay.graph

/**
  * @author Clément Garbay
  *
  * TODO: avoid code duplication ?
  */

case class NodeDirected[T](id: T, successors: Set[(T, Double)]) {

  val successorsIds: Set[T] = successors.map(_._1)

  def distanceWith(nodeId: T): Option[Double] =
    successors
      .filter(_._1 == nodeId)
      .map(_._2)
      .headOption

}

object NodeDirected {

  /**
    * Helper to create NodeDirected without distance.
    * By default, the distance is 1.
    */
  implicit def apply[T](id: T, successors: => Set[T]): NodeDirected[T] =
    NodeDirected(id, successors.map(nodeId => (nodeId, 1.0)))

}


case class NodeUndirected[T](id: T, neighbors: Set[(T, Double)]) {

  val neighborsIds: Set[T] = neighbors.map(_._1)

  def distanceWith(nodeId: String): Option[Double] =
    neighbors
      .filter(_._1 == nodeId)
      .map(_._2)
      .headOption

}

object NodeUndirected {

  /**
    * Helper to create NodeDirected without distance.
    * By default, the distance is 1.
    */
  implicit def apply[T](id: T, neighbors: => Set[T]): NodeUndirected[T] =
    NodeUndirected(id, neighbors.map(nodeId => (nodeId, 1.0)))

}


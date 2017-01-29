package fr.clementgarbay.graph

/**
  * @author Clément Garbay
  *
  * TODO: avoid code duplication ?
  */

case class NodeDirected[T](id: T, successors: Set[SemiArc[T]]) {

  val successorsIds: Set[T] = successors.map(_.to)

  def distanceWith(nodeId: T): Option[Double] =
    successors
      .filter(_.to == nodeId)
      .map(_.distance)
      .headOption

}

object NodeDirected {

  implicit def apply[T](id: T, successors: => Set[T]): NodeDirected[T] =
    NodeDirected(id, successors.map(nodeId => SemiArc(nodeId)))

}


case class NodeUndirected[T](id: T, neighbors: Set[SemiEdge[T]]) {

  val neighborsIds: Set[T] = neighbors.map(_.to)

  def distanceWith(nodeId: String): Option[Double] =
    neighbors
      .filter(_.to == nodeId)
      .map(_.distance)
      .headOption

}

object NodeUndirected {

  implicit def apply[T](id: T, neighbors: => Set[T]): NodeUndirected[T] =
    NodeUndirected(id, neighbors.map(nodeId => SemiEdge(nodeId)))

}


package fr.clementgarbay.graph
package adjacency_matrix

/**
  * @author Clément Garbay
  * @author Anaël Chardan
  *
  * TODO: implement with distances
  */
case class AdjacencyMatrixDirectedGraph[T](matrix: Map[T, Map[T, Int]]) extends IDirectedGraph[T] {

  override val nbNodes: Int = matrix.size
  override lazy val arcs: List[Arc[T]] =
    matrix.collect({
      case (from, neighbors) => neighbors.collect({
        case (to, value) if value == 1 => List(Arc(from, to), Arc(to, from))
      }).toSet.flatten
    }).flatten.toList

  override lazy val nbArcs: Int = matrix.map(_._2.count(_._2 == 1)).sum // arcs.size
  override lazy val nodesIds: List[T] = matrix.keys.toList

  override val toAdjacencyMatrix: Map[T, Map[T, Int]] = matrix

  override lazy val inverse: IDirectedGraph[T] = {
    AdjacencyMatrixDirectedGraph(
      matrix.map {
        case (from, line) => from -> line.map {
          case (to, _) => to -> isArc(to, from).toInt
        }
      }
    )
  }

  override lazy val toUndirectedGraph: AdjacencyMatrixUndirectedGraph[T] =
    AdjacencyMatrixUndirectedGraph(
      matrix.map {
        case (from, line) => from -> line.map {
          case (to, _) => to -> (isArc(to, from) || isArc(from, to)).toInt
        }
      }
    )

  override def getNodes: List[T] = matrix.keys.toList

  override def isArc(from: T, to: T): Boolean = matrix.lift(from).flatMap(_.lift(to)).getOrElse(0) == 1

  override def addArc(from: T, to: T, distance: Double): IDirectedGraph[T] = addArc(from, to)

  override def addArc(from: T, to: T): IDirectedGraph[T] = update(from, to, 1)

  override def removeArc(from: T, to: T): IDirectedGraph[T] = update(from, to, 0)

  override def getDistance(from: T, to: T): Double = 1

  override def getSuccessors(nodeId: T): Set[SemiArc[T]] =
    getSuccessorsIds(nodeId).map(node => SemiArc(node))

  override def getSuccessorsIds(nodeId: T): Set[T] =
    matrix.lift(nodeId).getOrElse(Map.empty).collect({
      case (node, isArc) if isArc == 1 => node
    }).toSet

  override def getPredecessors(nodeId: T): Set[SemiArc[T]] = getPredecessorsIds(nodeId).map(node => SemiArc(node))

  override def getPredecessorsIds(nodeId: T): Set[T] = matrix.keys.filter(isArc(_, nodeId)).toSet

  private def update(from: T, to: T, value: Int): IDirectedGraph[T] =
    AdjacencyMatrixDirectedGraph(
      matrix.map {
        case (index, successors) if index == from && successors.contains(index) => (index, successors.updated(to, value))
        case (index, successors) => (index, successors)
      }
    )
}

object AdjacencyMatrixDirectedGraph {

  implicit def apply[T](graph: IDirectedGraph[T]): AdjacencyMatrixDirectedGraph[T] =
    AdjacencyMatrixDirectedGraph(
      graph.getNodes.map(from => from -> graph.getNodes.map(to => (to, graph.isArc(from, to).toInt)).toMap).toMap
    )

  implicit def apply(matrix: List[List[Int]]): AdjacencyMatrixDirectedGraph[Int] = {
    AdjacencyMatrixDirectedGraph(matrix.zipWithIndex.map(_.swap).toMap.mapValues(_.zipWithIndex.map(_.swap).toMap))
  }
}
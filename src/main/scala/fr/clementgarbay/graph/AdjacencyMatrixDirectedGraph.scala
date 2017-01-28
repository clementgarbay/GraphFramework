package fr.clementgarbay.graph

/**
  * @author Clément Garbay
  *
  * TODO: implement with distances
  */
case class AdjacencyMatrixDirectedGraph(matrix: List[List[Int]]) extends IDirectedGraph[Int] {

  override val nbNodes: Int = matrix.size
  override lazy val arcs: Set[(Int, Int, Double)] =
    matrix.zipWithIndex.collect({
      case (successors, j) => successors.zipWithIndex.collect({
        case (value, i) if value == 1 => (j, i, 1.0)
      }).toSet
    }).toSet.flatten
  override lazy val nbArcs: Int = matrix.map(_.count(_ == 1)).sum // arcs.size
  override lazy val nodesIds: Set[Int] = matrix.indices.toSet

  override val toAdjacencyMatrix: List[List[Int]] = matrix

  override lazy val inverse: IDirectedGraph[Int] = AdjacencyMatrixDirectedGraph(matrix.transpose)

  override lazy val toUndirectedGraph: AdjacencyMatrixUndirectedGraph =
    AdjacencyMatrixUndirectedGraph(
      (0 until nbNodes).toList.map { i =>
        (0 until nbNodes).toList.map { j =>
          (isArc(i, j) || isArc(j, i)).toInt
        }
      }
    )

  override def isArc(from: Int, to: Int): Boolean = matrix.lift(from).flatMap(_.lift(to)).getOrElse(0) == 1

  override def addArc(from: Int, to: Int, distance: Double): IDirectedGraph[Int] = addArc(from, to)

  override def addArc(from: Int, to: Int): IDirectedGraph[Int] = update(from, to, 1)

  override def removeArc(from: Int, to: Int): IDirectedGraph[Int] = update(from, to, 0)

  override def getSuccessors(nodeId: Int): Set[(Int, Double)] =
    getSuccessorsIds(nodeId).map(node => (node, 1.0))

  override def getSuccessorsIds(nodeId: Int): Set[Int] =
    matrix.lift(nodeId)
      .map(_.zipWithIndex).getOrElse(Set.empty)
      .collect { case (value, index) if value == 1 => index }
      .toSet

  override def getPredecessors(nodeId: Int): Set[(Int, Double)] = getPredecessorsIds(nodeId).map(node => (node, 1.0))

  override def getPredecessorsIds(nodeId: Int): Set[Int] = matrix.indices.filter(isArc(_, nodeId)).toSet

  private def update(from: Int, to: Int, value: Int): IDirectedGraph[Int] =
    AdjacencyMatrixDirectedGraph(
      matrix.zipWithIndex.map {
        case (successors, i) if i == from && to < successors.length => successors.updated(to, value)
        case (successors, _) => successors
      }
    )

}

object AdjacencyMatrixDirectedGraph {

  implicit def apply(graph: IDirectedGraph[Int]): AdjacencyMatrixDirectedGraph =
    AdjacencyMatrixDirectedGraph(
      (0 until graph.nbNodes).toList.map { i =>
        (0 until graph.nbNodes).toList.map { j =>
          graph.isArc(i, j).toInt
        }
      }
    )

}
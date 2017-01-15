package fr.clementgarbay.graph

/**
  * @author Clément Garbay
  */
case class AdjacencyMatrixDirectedGraph(matrix: List[List[Int]]) extends IDirectedGraph {

  override val nbNodes: Int = matrix.size
  override lazy val nbArcs: Int = matrix.map(_.count(_ == 1)).sum

  override val toAdjacencyMatrix: List[List[Int]] = matrix

  override lazy val computeInverse: IDirectedGraph = AdjacencyMatrixDirectedGraph(matrix.transpose)

  override lazy val toUndirectedGraph: AdjacencyMatrixUndirectedGraph =
    AdjacencyMatrixUndirectedGraph(
      (0 until nbNodes).toList.map { i =>
        (0 until nbNodes).toList.map { j =>
          (isArc(i, j) || isArc(j, i)).toInt
        }
      }
    )

  override def isArc(from: Int, to: Int): Boolean = matrix.lift(from).flatMap(_.lift(to)).getOrElse(0) == 1

  override def addArc(from: Int, to: Int): IDirectedGraph = update(from, to, 1)

  override def removeArc(from: Int, to: Int): IDirectedGraph = update(from, to, 0)

  override def getSuccessors(node: Int): Set[Int] =
    matrix.lift(node)
      .map(_.zipWithIndex).getOrElse(Set.empty)
      .collect { case (value, index) if value == 1 => index }
      .toSet

  override def getPredecessors(node: Int): Set[Int] = matrix.indices.filter(isArc(_, node)).toSet

  private def update(from: Int, to: Int, value: Int): IDirectedGraph =
    AdjacencyMatrixDirectedGraph(
      matrix.zipWithIndex.map {
        case (neighbors, i) if i == from && to < neighbors.length => neighbors.updated(to, value)
        case (neighbors, _) => neighbors
      }
    )
}

object AdjacencyMatrixDirectedGraph {

  implicit def apply(graph: IDirectedGraph): AdjacencyMatrixDirectedGraph =
    AdjacencyMatrixDirectedGraph(
      (0 until graph.nbNodes).toList.map { i =>
        (0 until graph.nbNodes).toList.map { j =>
          graph.isArc(i, j).toInt
        }
      }
    )

}
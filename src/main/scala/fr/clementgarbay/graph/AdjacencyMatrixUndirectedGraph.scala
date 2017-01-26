package fr.clementgarbay.graph

/**
  * @author Clément Garbay
  */
case class AdjacencyMatrixUndirectedGraph(matrix: List[List[Int]]) extends IUndirectedGraph {

  override val nbNodes: Int = matrix.size
  override lazy val nbEdges: Int = matrix.map(_.count(_ == 1)).sum / 2
  override lazy val nodesList: Set[Int] = matrix.indices.toSet

  override val toAdjacencyMatrix: List[List[Int]] = matrix

  override def isEdge(from: Int, to: Int): Boolean =
    matrix.lift(from).flatMap(_.lift(to)).getOrElse(0) == 1 ||
    matrix.lift(to).flatMap(_.lift(from)).getOrElse(0) == 1

  override def addEdge(from: Int, to: Int): IUndirectedGraph = update(from, to, 1)

  override def removeEdge(from: Int, to: Int): IUndirectedGraph = update(from, to, 0)

  override def getNeighbors(node: Int): Set[Int] =
    matrix.lift(node)
      .map(_.zipWithIndex).getOrElse(Set.empty)
      .collect { case (value, index) if value == 1 => index }
      .toSet

  private def update(from: Int, to: Int, value: Int): IUndirectedGraph =
    AdjacencyMatrixUndirectedGraph(matrix.zipWithIndex.map {
      case (neighbors, i) if i == from && to < neighbors.length => neighbors.updated(to, value)
      case (neighbors, j) if j == to && from < neighbors.length => neighbors.updated(from, value)
      case (neighbors, _) => neighbors
    })
}

object AdjacencyMatrixUndirectedGraph {

  implicit def apply(graph: IUndirectedGraph): AdjacencyMatrixUndirectedGraph =
    AdjacencyMatrixUndirectedGraph(
      (0 until graph.nbNodes).toList.map { i =>
        (0 until graph.nbNodes).toList.map { j =>
          graph.isEdge(i, j).toInt
        }
      }
    )

}
package fr.clementgarbay.graph

/**
  * @author Clément Garbay
  */
case class AdjacencyMatrixUndirectedGraph(matrix: List[List[Int]]) extends IUndirectedGraph {

  override val nbNodes: Int = matrix.size
  override val nbEdges: Int = matrix.map(_.count(_ == 1)).sum / 2

  override def isEdge(from: Int, to: Int): Boolean =
    matrix.lift(from).flatMap(_.lift(to)).getOrElse(0) == 1 ||
    matrix.lift(to).flatMap(_.lift(from)).getOrElse(0) == 1

  override def addEdge(from: Int, to: Int): IUndirectedGraph = update(from, to, 1)

  override def removeEdge(from: Int, to: Int): IUndirectedGraph = update(from, to, 0)

  override def getNeighbors(node: Int): Set[Int] = matrix.find(_ == node).filter(_ == 1).map(_.toSet).getOrElse(Set.empty)

  override def toAdjacencyMatrix: List[List[Int]] = matrix

  def update(from: Int, to: Int, value: Int): IUndirectedGraph =
    AdjacencyMatrixUndirectedGraph(matrix.zipWithIndex.map {
      case (neighbors, i) if i == from && neighbors.length < to => neighbors.updated(to, value)
      case (neighbors, j) if j == to && neighbors.length < from => neighbors.updated(from, value)
      case (neighbors, _) => neighbors
    })
}

object AdjacencyMatrixUndirectedGraph {

  def apply(undirectedGraph: IUndirectedGraph): AdjacencyMatrixUndirectedGraph = {
    new AdjacencyMatrixUndirectedGraph(
      (0 until undirectedGraph.nbNodes)
        .map({ i => undirectedGraph.getNeighbors(i).toList }) // TODO: add 0
        .toList
    )
  }

}
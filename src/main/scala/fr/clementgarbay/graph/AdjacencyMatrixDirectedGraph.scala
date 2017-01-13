package fr.clementgarbay.graph

/**
  * @author Clément Garbay
  */
case class AdjacencyMatrixDirectedGraph(matrix: List[List[Int]]) extends IDirectedGraph {

  override val nbNodes: Int = matrix.size
  override val nbArcs: Int = matrix.map(_.count(_ == 1)).sum / 2

  override def isArc(from: Int, to: Int): Boolean = matrix.lift(from).flatMap(_.lift(to)).getOrElse(0) == 1

  override def addArc(from: Int, to: Int): IDirectedGraph = update(from, to, 1)

  override def removeArc(from: Int, to: Int): IDirectedGraph = update(from, to, 0)

  override def getSuccessors(node: Int): Set[Int] = matrix.lift(node).getOrElse(List.empty).toSet

  override def getPredecessors(node: Int): Set[Int] = matrix.zipWithIndex.flatMap({
    case (neighbors, j) => neighbors.zipWithIndex.map {
      case (neighbor, i) if i == node && neighbor == 1 => j
    }
  }).toSet

  override def computeInverse: IDirectedGraph = ???

  override def toAdjacencyMatrix: List[List[Int]] = matrix

  def update(from: Int, to: Int, value: Int): IDirectedGraph =
    AdjacencyMatrixDirectedGraph(
      matrix.zipWithIndex.map {
        case (neighbors, i) if i == from && neighbors.length < to => neighbors.updated(to, value)
        case (neighbors, _) => neighbors
      }
    )
}

object AdjacencyMatrixDirectedGraph {

  def apply(directedGraph: IDirectedGraph): AdjacencyMatrixDirectedGraph = {
    new AdjacencyMatrixDirectedGraph(
      (0 until directedGraph.nbNodes)
        .map({ i => directedGraph.getSuccessors(i).toList }) // TODO: add 0
        .toList
    )
  }

}
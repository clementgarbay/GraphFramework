package fr.clementgarbay.graph

/**
  * @author Clément Garbay
  *
  * TODO: implement with distances
  */
case class AdjacencyMatrixUndirectedGraph(matrix: List[List[Int]]) extends IUndirectedGraph[Int] {

  override val nbNodes: Int = matrix.size
  override lazy val edges: List[Edge[Int]] =
    matrix.zipWithIndex.collect({
      case (neighbors, j) => neighbors.zipWithIndex.collect({
        case (value, i) if value == 1 => List(Edge(i, j), Edge(j, i))
      }).toSet.flatten
    }).flatten
  override lazy val nbEdges: Int = matrix.map(_.count(_ == 1)).sum / 2 // edges.size
  override lazy val nodesIds: List[Int] = matrix.indices.toList

  override val toAdjacencyMatrix: List[List[Int]] = matrix

  override def isEdge(from: Int, to: Int): Boolean =
    matrix.lift(from).flatMap(_.lift(to)).getOrElse(0) == 1 ||
    matrix.lift(to).flatMap(_.lift(from)).getOrElse(0) == 1

  override def addEdge(from: Int, to: Int): IUndirectedGraph[Int] = update(from, to, 1)

  override def addEdge(from: Int, to: Int, distance: Double): IUndirectedGraph[Int] = addEdge(from, to)

  override def removeEdge(from: Int, to: Int): IUndirectedGraph[Int] = update(from, to, 0)

  override def getNeighbors(nodeId: Int): Set[SemiEdge[Int]] = getNeighborsIds(nodeId).map(node => SemiEdge(node))

  override def getNeighborsIds(nodeId: Int): Set[Int] =
    matrix.lift(nodeId)
      .map(_.zipWithIndex).getOrElse(Set.empty)
      .collect { case (value, index) if value == 1 => index }
      .toSet

  private def update(from: Int, to: Int, value: Int): IUndirectedGraph[Int] =
    AdjacencyMatrixUndirectedGraph(matrix.zipWithIndex.map {
      case (neighbors, i) if i == from && to < neighbors.length => neighbors.updated(to, value)
      case (neighbors, j) if j == to && from < neighbors.length => neighbors.updated(from, value)
      case (neighbors, _) => neighbors
    })

}

object AdjacencyMatrixUndirectedGraph {

  implicit def apply(graph: IUndirectedGraph[Int]): AdjacencyMatrixUndirectedGraph =
    AdjacencyMatrixUndirectedGraph(
      (0 until graph.nbNodes).toList.map { i =>
        (0 until graph.nbNodes).toList.map { j =>
          graph.isEdge(i, j).toInt
        }
      }
    )

}
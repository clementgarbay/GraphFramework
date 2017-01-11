package fr.clementgarbay.graph

/**
  * @author Clément Garbay
  */
case class AdjacencyListUndirectedGraph(nodes: List[NodeUndirected]) extends IUndirectedGraph {


//  def this(matrix: List[List[Int]]) = {
//    matrix.foldLeft(List.empty[NodeUndirected]) { (acc, neighbours) =>
//      NodeUndirected(1, neighbours) :: acc
//    }
//  }

  /**
    * @inheritdoc
    */
  override def getNbEdges: Int = nodes.map(_.neighbours.size).sum / 2

  /**
    * @inheritdoc
    */
  override def isEdge(x: Int, y: Int): Boolean = nodes.exists(node => node.id == x && node.neighbours.exists(neighbour => neighbour.id == y))

  /**
    * @inheritdoc
    */
  override def removeEdge(x: Int, y: Int): AdjacencyListUndirectedGraph = {
    AdjacencyListUndirectedGraph(nodes.map {
      case node if node.id == x =>
        node.copy(neighbours = node.neighbours.filterNot(_.id == y))
      case node if node.id == y =>
        node.copy(neighbours = node.neighbours.filterNot(_.id == x))
      case node => node
    })
  }

  /**
    * @inheritdoc
    */
  override def addEdge(x: Int, y: Int): AdjacencyListUndirectedGraph = ???

  /**
    * @inheritdoc
    */
  override def getNeighbors(x: Int): List[Int] = ???

  /**
    * @inheritdoc
    */
  override def getNbNodes: Int = ???

  /**
    * @inheritdoc
    */
  override def toAdjacencyMatrix: List[List[Int]] = ???


}

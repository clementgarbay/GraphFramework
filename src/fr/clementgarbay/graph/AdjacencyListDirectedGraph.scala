package fr.clementgarbay.graph

/**
  * @author Clément Garbay
  */
case class AdjacencyListDirectedGraph(nodes: List[NodeDirected]) extends IDirectedGraph {
  /**
    * @inheritdoc
    */
  override def getNbArcs: Int = ???

  /**
    * @inheritdoc
    */
  override def isArc(from: Int, to: Int): Boolean = ???

  /**
    * @inheritdoc
    */
  override def removeArc(from: Int, to: Int): Unit = ???

  /**
    * @inheritdoc
    */
  override def addArc(from: Int, to: Int): Unit = ???

    /**
      * @inheritdoc
      */
  override def getSuccessors(x: Int): List[Int] = ???

    /**
      * @inheritdoc
      */
  override def getPredecessors(x: Int): List[Int] = ???

  /**
    * @inheritdoc
    */
  override def computeInverse: IDirectedGraph = ???

  /**
    * @inheritdoc
    */
  override def getNbNodes: Int = ???

  /**
    * @inheritdoc
    */
  override def toAdjacencyMatrix: List[List[Int]] = ???
}

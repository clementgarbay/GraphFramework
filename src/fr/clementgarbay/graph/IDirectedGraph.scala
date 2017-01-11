package fr.clementgarbay.graph

/**
  * @author Clément Garbay
  */
trait IDirectedGraph extends IGraph {

  /**
    * @return The number of arcs in the graph
    */
  def getNbArcs: Int

  /**
    * @param from
    * @param to
    * @return True iff arc (from,to) figures in the graph
    */
  def isArc(from: Int, to: Int): Boolean

  /**
    * The arc (from,to), if exists
    * @param from
    * @param to
    */
  def removeArc(from: Int, to: Int)

  /**
    * Adds the arc (from,to) if it is not already present in the graph, requires from /= to
    * @param from
    * @param to
    */
  def addArc(from: Int, to: Int)

  /**
    * @param x
    * @return A new int list representing successors of node x
    */
  def getSuccessors(x: Int): List[Int]

  /**
    * @param x
    * @return A new int list representing predecessors of node x
    */
  def getPredecessors(x: Int): List[Int]

  /**
    * Computes the inverse graph
    * @return
    */
  def computeInverse: IDirectedGraph
}

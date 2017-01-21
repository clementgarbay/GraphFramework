package fr.clementgarbay.graph

/**
  * @author Clément Garbay
  */
trait IDirectedGraph extends IGraph {

  /**
    * The number of arcs in the graph
    */
  val nbArcs: Int

  /**
    * The inverse graph
    */
  val computeInverse: IDirectedGraph

  /**
    * The corresponding undirected graph from the directed graph
    */
  val toUndirectedGraph: IUndirectedGraph

  /**
    * Tests if two nodes are an arc
    *
    * @param from The first node
    * @param to   The second node
    * @return True iff arc (from,to) figures in the graph
    */
  def isArc(from: Int, to: Int): Boolean

  /**
    * Adds the arc (from,to) if it is not already present in the graph, requires from /= to
    *
    * @param from The first node
    * @param to   The second node
    * @return     A new graph with modification
    */
  def addArc(from: Int, to: Int): IDirectedGraph

  /**
    * Remove the arc (from,to), if exists
    *
    * @param from The first node
    * @param to   The second node
    * @return     A new graph with modification
    */
  def removeArc(from: Int, to: Int): IDirectedGraph

  /**
    * Get successors of a specific node
    *
    * @param node The related node
    * @return     A int list representing successors of node
    */
  def getSuccessors(node: Int): Set[Int]

  /**
    * Get predecessors of a specific node
    *
    * @param node The related node
    * @return     A int list representing predecessors of node
    */
  def getPredecessors(node: Int): Set[Int]

  /**
    * Explore a graph with the depth first search algorithm
    *
    * @param startingNode The starting node
    * @return             All nodes reachable from the starting node
    */
  def depthFirstSearch(startingNode: Int): Set[Int] = {
    def depthFirstSearchRec(node: Int, visited: Set[Int]): Set[Int] = {
      if (visited contains node) visited
      else getSuccessors(node).foldLeft(visited + node)((visitedNodes, successor) => depthFirstSearchRec(successor, visitedNodes))
    }

    depthFirstSearchRec(startingNode, Set())
  }

  /**
    * Explore a graph with the breadth first search algorithm
    *
    * @param startingNode The starting node
    * @return             All nodes reachable from the starting node
    */
  def breadthFirstSearch(startingNode: Int): Set[Int] = {
    def breadthFirstSearchRec(toVisit: Set[Int], visited: Set[Int]): Set[Int] = {
      val successors = toVisit.flatMap(getSuccessors).diff(visited)
      if (successors.isEmpty) visited
      else breadthFirstSearchRec(successors, visited ++ successors)
    }

    breadthFirstSearchRec(Set(startingNode), Set(startingNode))
  }

}

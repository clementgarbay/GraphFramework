package fr.clementgarbay.graph

/**
  * @author Clément Garbay
  */
trait IUndirectedGraph extends IGraph {

  /**
    * The number of edges in the graph
    */
  val nbEdges: Int

  /**
    * Check the connectivity of the graph
    */
  val isConnected: Boolean = depthFirstSearch(0).size == nbNodes

  /**
    * Tests if two nodes are a edge
    *
    * @param from The first node
    * @param to   The second node
    * @return     True if there is an edge between x and y
    */
  def isEdge(from: Int, to: Int): Boolean

  /**
    * Adds edge (from,to) if not already present, requires from /= to
    *
    * @param from The first node
    * @param to   The second node
    * @return     A new graph with modification
    */
  def addEdge(from: Int, to: Int): IUndirectedGraph

  /**
    * Removes edge (from,to) if exists
    *
    * @param from The first node
    * @param to   The second node
    * @return     A new graph with modification
    */
  def removeEdge(from: Int, to: Int): IUndirectedGraph

  /**
    * Get neighbors of a specific node
    *
    * @param node The related node
    * @return     A int list representing neighbors of node
    */
  def getNeighbors(node: Int): Set[Int]

  /**
    * Explore a graph with the depth first search algorithm
    *
    * @param startingNode The starting node
    * @return             All nodes reachable from the starting node
    */
  def depthFirstSearch(startingNode: Int): Set[Int] = {
    def depthFirstSearchRec(node: Int, visited: Set[Int]): Set[Int] = {
      if (visited contains node) visited
      else getNeighbors(node).foldLeft(visited + node)((visitedNodes, neighbor) => depthFirstSearchRec(neighbor, visitedNodes))
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
      val neighbors = toVisit.flatMap(getNeighbors).diff(visited)
      if (neighbors.isEmpty) visited
      else breadthFirstSearchRec(neighbors, visited ++ neighbors)
    }

    breadthFirstSearchRec(Set(startingNode), Set(startingNode))
  }

//  def exploreGraph(): Set[IUndirectedGraph] = {
//
//
//    def exploreNode(node: Int, a: Set[Int]): Unit = {
//
//    }
//  }
//
//  void explorerGraphe() {
//    Set<Sommet> atteint = new HashSet<Sommet>()
//    visit[] = 0
//    for (Sommet s : sommets) {
//      if (!atteint.contains(s)) {
//        explorerSommet(s, atteint)
//      }
//    }
//  }
//
//  void explorerSommet(Sommet s, Set<Sommets> a) {
//    a.add(s)
//
//    for (Sommet t : s.voisins()) {
//      if (!a.contains(t)) {
//        explorerSommet(t,a)
//      }
//    }
//    fin.add(s)
//  }

//  def explore(): Set[Set[Int]] = {
//
//    val first = depthFirstSearch(0)
//    var res = Set(first)
//
//    var notVisited = (0 until nbNodes).diff(first.toSeq)
//
//    while (notVisited.nonEmpty) {
//      val node = notVisited.collectFirst({case x: Int => x})
//      val visited = depthFirstSearch(node.get)
//      res = res ++ visited
//      notVisited = (0 until nbNodes).diff(visited.toSeq)
//    }
//
//    res
//  }

}

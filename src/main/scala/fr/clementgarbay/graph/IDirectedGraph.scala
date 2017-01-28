package fr.clementgarbay.graph

import scala.collection.{SortedSet, mutable}
import scala.collection.immutable.ListMap

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
  val inverse: IDirectedGraph

  /**
    * The corresponding undirected graph from the directed graph
    */
  val toUndirectedGraph: IUndirectedGraph

  /**
    * Check the strong connectivity of the graph
    */
  val isConnected: Boolean = depthFirstSearch(0).size == nbNodes && inverse.depthFirstSearch(0).size == nbNodes

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
    * @return A new graph with modification
    */
  def addArc(from: Int, to: Int): IDirectedGraph

  /**
    * Remove the arc (from,to), if exists
    *
    * @param from The first node
    * @param to   The second node
    * @return A new graph with modification
    */
  def removeArc(from: Int, to: Int): IDirectedGraph

  /**
    * Get successors of a specific node
    *
    * @param node The related node
    * @return A int list representing successors of node
    */
  def getSuccessors(node: Int): Set[Int]

  /**
    * Get predecessors of a specific node
    *
    * @param node The related node
    * @return A int list representing predecessors of node
    */
  def getPredecessors(node: Int): Set[Int]

  /**
    * Explore a graph with the depth first search algorithm
    *
    * @param startingNode The starting node
    * @return All nodes reachable from the starting node
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
    * @return All nodes reachable from the starting node
    */
  def breadthFirstSearch(startingNode: Int): Set[Int] = {
    def breadthFirstSearchRec(toVisit: Set[Int], visited: Set[Int]): Set[Int] = {
      val successors = toVisit.flatMap(getSuccessors).diff(visited)
      if (successors.isEmpty) visited
      else breadthFirstSearchRec(successors, visited ++ successors)
    }

    breadthFirstSearchRec(Set(startingNode), Set(startingNode))
  }

  /**
    * GO AWAY VERY UGLY METHOD
    *
    * @param startingNode
    * @return
    */
  def baseDfs(startingNode: Int, listToFollow: Set[Int] = nodesList): (Map[Int, Int], Map[Int, Int], Set[Set[Int]]) = {
    var increment: Int = 0
    var start: Map[Int, Int] = Map.empty
    var end: Map[Int, Int] = Map.empty

    def nodeDfs(node: Int, visited: Set[Int]): Set[Int] = {
      var currentVisit = visited + node

      increment += 1
      start += node -> increment

      for (successor <- getSuccessors(node) if !(currentVisit contains successor)) {
        currentVisit = currentVisit ++ nodeDfs(successor, currentVisit)

      }

      increment += 1
      end += node -> increment

      currentVisit
    }

    var visited: Set[Set[Int]] = Set(nodeDfs(startingNode, Set.empty))
    var nodeToVisit = diffKeepingOrder(listToFollow, visited.flatten)

    print("First deep done")

    for(node <- nodeToVisit if !(visited.flatten contains node)) {
      visited = visited + nodeDfs(node, visited.flatten).diff(visited.flatten)
    }

    (start, end, visited)
  }

  /**
    * The scala diff method not keep the order of sets
    *
    * @param biggestSet
    * @param smallestSet
    * @return
    */
  def diffKeepingOrder(biggestSet: Set[Int], smallestSet: Set[Int]): mutable.Queue[Int] = {
    var resultedSet: mutable.Queue[Int] = mutable.Queue.empty

    for (element <- biggestSet if !(smallestSet contains element)) {
      resultedSet.enqueue(element)
    }

    resultedSet
  }

  /**
    * Get all strong connected components
    *
    * @param startingNode
    * @return
    */
  def computeStrongConnectivity(startingNode: Int): Set[Set[Int]] = {
    val sortedEnd = ListMap(baseDfs(startingNode)._2.toSeq.sortWith(_._2 > _._2):_*)

    val strongComponents = inverse.baseDfs(sortedEnd.head._1, sortedEnd.keys.toSet)

    strongComponents._3
  }

}

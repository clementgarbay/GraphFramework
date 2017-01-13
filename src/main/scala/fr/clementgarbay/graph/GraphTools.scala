package fr.clementgarbay.graph

import scala.util.Random

/**
  * @author Clément Garbay
  */
object GraphTools {

  /**
    * Generate randomly a list of edges (tuple of vertex ids)
    *
    * @param number     the desired number of edges
    * @param graphOrder the order of the graph
    * @return
    */
  def getRandomEdges(number: Int, graphOrder: Int): Set[(Int,Int)] = {
    // Choose the most efficient method
    if ((graphOrder * graphOrder - graphOrder) / number > 2) {
      (1 to number)
        .foldLeft(Set.empty[(Int,Int)]) { (acc: Set[(Int,Int)], _) =>
          acc + Stream
            .continually((Random.nextInt(graphOrder), Random.nextInt(graphOrder)))
            .dropWhile(coor => acc.contains(coor) || (coor._1 == coor._2))
            .collectFirst({case x: (Int,Int) => x})
            .get
        }
    } else {
      Random.shuffle((for (i <- 0 until graphOrder; j <- 0 until graphOrder if i != j) yield (i,j)).toSet).take(number)
    }
  }

  /**
    * Generate randomly a directed graph
    *
    * @param order    the order of the graph
    * @param nbEdges  the desired number of edges
    * @return
    */
  def generateDirectedGraph(order: Int, nbEdges: Int): AdjacencyListDirectedGraph = {
    val edges: Set[(Int,Int)] = getRandomEdges(nbEdges, order)

    val nodes: Set[NodeDirected] = (0 until order).map(nodeId => {
      NodeDirected(nodeId, edges.filter(_._1 == nodeId).map(_._2))
    }).toSet

    AdjacencyListDirectedGraph(nodes)
  }

  /**
    * Generate randomly an undirected graph
    *
    * @param order    the order of the graph
    * @param nbEdges  the desired number of edges
    * @return
    */
  def generateUndirectedGraph(order: Int, nbEdges: Int): AdjacencyListUndirectedGraph = {
    generateDirectedGraph(order,nbEdges).toUndirectedGraph
  }

  def generateGraphData(order: Int, nbEdges: Int, s: Boolean): List[List[Int]] = {
    val edges: Set[(Int,Int)] = getRandomEdges(order, nbEdges)
    var list = List.fill(order,order)(0)

    for (edge <- edges) {
      list = list.updated(edge._2, list(edge._2).updated(edge._1, 1))
      if (s) {
        list = list.updated(edge._1, list(edge._1).updated(edge._2, 1))
      }
    }

    list
  }

  def displayGraph(graph: List[List[Int]]) = {
    println(graph.map(_.mkString("  ")).mkString("\n"))
  }

  def displayGraph(graph: AdjacencyListDirectedGraph) = {
    println(graph.nodes.map(node => node.id + " -> " + node.successors.mkString(", ")).mkString("\n"))
  }

  def displayGraph(graph: AdjacencyListUndirectedGraph) = {
    println(graph.nodes.map(node => node.id + " -> " + node.neighbors.mkString(", ")).mkString("\n"))
  }

  //  def generateDirectedGraphV2(order: Int, nbEdges: Int): AdjacencyListDirectedGraph = {
  //    val edges: List[(Int,Int)] = getRandomEdges(nbEdges, order)
  //
  //    // Create all nodes without successors
  //    val nodes: List[NodeDirected] = (0 until order).map(nodeId => {
  //      NodeDirected(nodeId, List.empty)
  //    }).toList
  //
  //    // Update nodes's successors from edges list and return the new AdjacencyListDirectedGraph
  //    AdjacencyListDirectedGraph(nodes.map(nodeFrom => {
  //      val edgesStartingFromNode: List[(Int,Int)] = edges.filter(_._1 == nodeFrom.id)
  //
  //      if (edgesStartingFromNode.nonEmpty) {
  //        val nodesTo: List[NodeDirected] = edgesStartingFromNode.map(edge => nodes.find(_.id == edge._2).get)
  //        nodeFrom.copy(successors = nodesTo ::: nodeFrom.successors)
  //      } else {
  //        nodeFrom
  //      }
  //    }))
  //  }
}

package fr.clementgarbay.graph

import fr.clementgarbay.graph.adjacency_list.{AdjacencyListDirectedGraph, AdjacencyListUndirectedGraph}

import scala.util.Random

/**
  * @author Clément Garbay
  * @author Anaël Chardan
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
  def generateDirectedGraph(order: Int, nbEdges: Int): AdjacencyListDirectedGraph[Int] = {
    val edges: Set[(Int,Int)] = getRandomEdges(nbEdges, order)

    val nodes: List[NodeDirected[Int]] = (0 until order).map(nodeId => {
      NodeDirected(nodeId, edges.filter(_._1 == nodeId).map(_._2))
    }).toList

    AdjacencyListDirectedGraph(nodes)
  }

  /**
    * Generate randomly an undirected graph
    *
    * @param order    the order of the graph
    * @param nbEdges  the desired number of edges
    * @return
    */
  def generateUndirectedGraph(order: Int, nbEdges: Int): AdjacencyListUndirectedGraph[Int] = {
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

  def displayGraph(graph: List[List[Int]]): Unit = {
    println(graph.map(_.mkString("  ")).mkString("\n"))
  }

  def displayGraph[T](graph: AdjacencyListDirectedGraph[T]): Unit = {
    println(graph.nodes.map(node => node.id + " -> " + node.successors.mkString(", ")).mkString("\n"))
  }

  def displayGraph[T](graph: AdjacencyListUndirectedGraph[T]): Unit = {
    println(graph.nodes.map(node => node.id + " -> " + node.neighbors.mkString(", ")).mkString("\n"))
  }

  def time[R](block: => R): Long = {
    val t0 = System.nanoTime()
    val result = block
    // call-by-name
    val t1 = System.nanoTime()
    println("Elapsed time: " + (t1 - t0) + " ns or " + (t1 - t0) / 1000000 + " ms")
    t1 - t0
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

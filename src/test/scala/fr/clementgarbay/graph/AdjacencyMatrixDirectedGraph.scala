package fr.clementgarbay.graph

import org.scalatest.WordSpec

/**
  * @author Damien Raymond
  * @author Clément Garbay
  */
class AdjacencyMatrixDirectedGraphTest extends WordSpec {

  "A directed adjacency list graph with 4 nodes and 7 arcs" should {
    "have 4 nodes" in new ContextMatrixDirected {
      assert(graph.nbNodes == 4)
    }
    "have 6 arcs" in new ContextMatrixDirected {
      assert(graph.nbArcs == 6)
    }
    "return true" when {
      "isArc is called and arc exists" in new ContextMatrixDirected {
        assert(graph.isArc(1, 3))
      }
    }
    "return false" when {
      "isArc is called and arc doesn't exist" in new ContextMatrixDirected {
        assert(!graph.isArc(2, 3))
      }
    }
    "have 7 arcs" when {
      "addArc is called with a new arc" in new ContextMatrixDirected {
        assert(graph.addArc(2, 3).nbArcs == 7)
      }
    }
    "have 6 arcs" when {
      "addArc is called with not existing nodes" in new ContextMatrixDirected {
        assert(graph.addArc(5, 0).nbArcs == 6)
      }
    }
    "have 5 arcs" when {
      "removeArc is called" in new ContextMatrixDirected {
        assert(graph.removeArc(1, 3).nbArcs == 5)
      }
    }
    "have 6 arcs" when {
      "removeArc is called on not existing arc" in new ContextMatrixDirected {
        assert(graph.removeArc(4, 3).nbArcs == 6)
      }
    }
    "be a int list of 1, 2" when {
      "getSuccessors is called with 1 in parameter" in new ContextMatrixDirected {
        assert(graph.getSuccessors(0) == Set(1, 2))
      }
    }
    "be a int list of 3" when {
      "getSuccessors is called with 1 in parameter" in new ContextMatrixDirected {
        assert(graph.getSuccessors(1) == Set(3))
      }
    }
    "be a int list of 0" when {
      "getSuccessors is called with 2 in parameter" in new ContextMatrixDirected {
        assert(graph.getSuccessors(2) == Set(1))
      }
    }
    "be a int list of 1,3" when {
      "getPredecessors is called with 0 in parameter" in new ContextMatrixDirected {
        assert(graph.getPredecessors(0) == Set(3))
      }
    }
    "be a int list of 0,2" when {
      "getPredecessors is called with 1 in parameter" in new ContextMatrixDirected {
        assert(graph.getPredecessors(1) == Set(0, 2))
      }
    }
    "be the correct directed adjacency list graph inverse" when {
      "computeInverse is called" in new ContextMatrixDirected {
        assert(graph.computeInverse == graphInverse)
      }
    }
    "be the correct adjacency matrix representation (List of List)" when {
      "toAdjacencyMatrix is called" in new ContextMatrixDirected {
        assert(graph.toAdjacencyMatrix == graphMatrix)
      }
    }
    "initialize an undirected adjacency list graph" when {
      "from this directed adjacency list graph" in new ContextMatrixDirected {
        assert(graph.toUndirectedGraph == graphUndirected)
      }
    }
    "initialize a directed adjacency matrix graph" when {
      "from adjacency list representation" in new ContextMatrixDirected {
        assert(adjacencyMatrix == graph)
      }
    }
    "be a int list of all nodes (0,1,2,3)" when {
      "explore the graph with DFS algorithm starting with node 0" in new ContextMatrixDirected {
        assert(graph.depthFirstSearch(0) == Set(0,1,2,3))
      }
    }
    "be a int list of all nodes (0,1,2,3)" when {
      "explore the graph with BFS algorithm starting with node 0" in new ContextMatrixDirected {
        assert(graph.breadthFirstSearch(0) == Set(0,1,2,3))
      }
    }
    "be a int list of 0,1,2" when {
      "explore the graph `graph2` with DFS algorithm starting with node 0" in new ContextMatrixDirected {
        assert(graph2.depthFirstSearch(0) == Set(0,1,2))
      }
    }
    "be a int list of 0,1,2" when {
      "explore the graph `graph2` with BFS algorithm starting with node 0" in new ContextMatrixDirected {
        assert(graph2.breadthFirstSearch(0) == Set(0,1,2))
      }
    }
  }
}


trait ContextMatrixDirected {

  val graphMatrix = List(
    List(0, 1, 1, 0),
    List(0, 0, 0, 1),
    List(0, 1, 0, 0),
    List(1, 0, 1, 0)
  )

  val graphMatrix2 = List(
    List(0, 1, 1, 0),
    List(0, 0, 1, 0),
    List(0, 1, 0, 0),
    List(1, 0, 1, 0)
  )

  val graphMatrixInverse = List(
    List(0, 0, 0, 1),
    List(1, 0, 1, 0),
    List(1, 0, 0, 1),
    List(0, 1, 0, 0)
  )

  val adjacencyList = Map(
    (0, Set(1, 2)),
    (1, Set(3)),
    (2, Set(1)),
    (3, Set(0, 2))
  )

  val adjacencyListDirectedGraph: AdjacencyListDirectedGraph = AdjacencyListDirectedGraph.apply(adjacencyList)
  var adjacencyMatrix: AdjacencyMatrixDirectedGraph = AdjacencyMatrixDirectedGraph.apply(adjacencyListDirectedGraph)

  val graphMatrixUndirected = List(
    List(0, 1, 1, 1),
    List(1, 0, 1, 1),
    List(1, 1, 0, 1),
    List(1, 1, 1, 0)
  )

  val graph = AdjacencyMatrixDirectedGraph(graphMatrix)
  val graph2 = AdjacencyMatrixDirectedGraph(graphMatrix2)
  val graphInverse = AdjacencyMatrixDirectedGraph(graphMatrixInverse)
  val graphUndirected = AdjacencyMatrixUndirectedGraph(graphMatrixUndirected)
}
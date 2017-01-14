package fr.clementgarbay.graph

import org.scalatest._

/**
  * @author Clément Garbay
  */
class AdjacencyListDirectedGraphTest extends WordSpec {

  "A directed adjacency list graph with 4 nodes and 7 arcs" should {
    "have 4 nodes" in new ContextDirected {
      assert(graph.nbNodes == 4)
    }
    "have 7 arcs" in new ContextDirected {
      assert(graph.nbArcs == 7)
    }
    "return true" when {
      "isArc is called and arc exists" in new ContextDirected {
        assert(graph.isArc(1,3))
      }
    }
    "return false" when {
      "isArc is called and arc doesn't exist" in new ContextDirected {
        assert(!graph.isArc(2,3))
      }
    }
    "have 8 arcs" when {
      "addEdge is called with a new arc" in new ContextDirected {
        assert(graph.addArc(2,3).nbArcs == 8)
      }
    }
    "have 6 arcs" when {
      "removeArc is called" in new ContextDirected {
        assert(graph.removeArc(1,3).nbArcs == 6)
      }
    }
    "be a int list of 0,3" when {
      "getSuccessors is called with 1 in parameter" in new ContextDirected {
        assert(graph.getSuccessors(1) == Set(0,3))
      }
    }
    "be a int list of 1" when {
      "getSuccessors is called with 2 in parameter" in new ContextDirected {
        assert(graph.getSuccessors(2) == Set(1))
      }
    }
    "be a int list of 1,3" when {
      "getPredecessors is called with 0 in parameter" in new ContextDirected {
        assert(graph.getPredecessors(0) == Set(1,3))
      }
    }
    "be a int list of 0,2" when {
      "getPredecessors is called with 1 in parameter" in new ContextDirected {
        assert(graph.getPredecessors(1) == Set(0,2))
      }
    }
    "be the correct directed adjacency list graph inverse" when {
      "computeInverse is called" in new ContextDirected {
        assert(graph.computeInverse == graphInverse)
      }
    }
    "be the correct adjacency matrix representation (List of List)" when {
      "toAdjacencyMatrix is called" in new ContextDirected {
        assert(graph.toAdjacencyMatrix == graphMatrix)
      }
    }
    "initialize an undirected adjacency list graph" when {
      "from this directed adjacency list graph" in new ContextDirected {
        assert(graph.toUndirectedGraph == graphUndirected)
      }
    }
    "initialize a directed adjacency list graph" when {
      "from adjacency matrix representation" in new ContextDirected {
        assert(AdjacencyListDirectedGraph(graphMatrix) == graph)
      }
    }
  }

}

trait ContextDirected {

  val graph = AdjacencyListDirectedGraph(Set(
    NodeDirected(0, Set(1, 2)),
    NodeDirected(1, Set(0, 3)),
    NodeDirected(2, Set(1)),
    NodeDirected(3, Set(0, 2))
  ))

  val graphInverse = AdjacencyListDirectedGraph(Set(
    NodeDirected(0, Set(1, 3)),
    NodeDirected(1, Set(0, 2)),
    NodeDirected(2, Set(0, 3)),
    NodeDirected(3, Set(1))
  ))

  val graphUndirected = AdjacencyListUndirectedGraph(Set(
    NodeUndirected(0, Set(1, 2, 3)),
    NodeUndirected(1, Set(0, 2, 3)),
    NodeUndirected(2, Set(0, 1, 3)),
    NodeUndirected(3, Set(0, 1, 2))
  ))

  val graphMatrix = List(
    List(0, 1, 1, 0),
    List(1, 0, 0, 1),
    List(0, 1, 0, 0),
    List(1, 0, 1, 0)
  )

}
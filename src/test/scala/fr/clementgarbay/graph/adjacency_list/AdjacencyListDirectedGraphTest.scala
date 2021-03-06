package fr.clementgarbay.graph.adjacency_list

import fr.clementgarbay.graph.{NodeDirected, NodeUndirected}
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
      "addArc is called with a new arc" in new ContextDirected {
        assert(graph.addArc(2,3).nbArcs == 8)
      }
    }
    "have 7 arcs" when {
      "addArc is called with a not existing node" in new ContextDirected {
        assert(graph.addArc(4,0).nbArcs == 7)
      }
    }
    "have 6 arcs" when {
      "removeArc is called" in new ContextDirected {
        assert(graph.removeArc(1,3).nbArcs == 6)
      }
    }
    "have 7 arcs" when {
      "removeArc is called with a not existing arc" in new ContextDirected {
        assert(graph.removeArc(4,3).nbArcs == 7)
      }
    }
    "be a int list of 0,3" when {
      "getSuccessors is called with 1 in parameter" in new ContextDirected {
        assert(graph.getSuccessorsIds(1) == Set(0,3))
      }
    }
    "be a int list of 1" when {
      "getSuccessors is called with 2 in parameter" in new ContextDirected {
        assert(graph.getSuccessorsIds(2) == Set(1))
      }
    }
    "be a int list of 1,3" when {
      "getPredecessors is called with 0 in parameter" in new ContextDirected {
        assert(graph.getPredecessorsIds(0) == Set(1,3))
      }
    }
    "be a int list of 0,2" when {
      "getPredecessors is called with 1 in parameter" in new ContextDirected {
        assert(graph.getPredecessorsIds(1) == Set(0,2))
      }
    }
    "be the correct directed adjacency list graph inverse" when {
      "computeInverse is called" in new ContextDirected {
        assert(graph.inverse == graphInverse)
      }
    }
    "be the correct adjacency matrix representation (List of List)" when {
      "toAdjacencyMatrix is called" in new ContextDirected {
        assert(graph.toAdjacencyMatrix == graphMatrixMap)
      }
    }
    "initialize an undirected adjacency list graph" when {
      "from this directed adjacency list graph" in new ContextDirected {
        assert(graph.toUndirectedGraph == graphUndirected)
      }
    }
    "initialize a directed adjacency list graph" when {
      "from adjacency matrix representation" in new ContextDirected {
        assert(AdjacencyListDirectedGraph(graphMatrixList) == graph)
      }
    }
    "be a int list of all nodes (0,1,2,3)" when {
      "explore the graph with DFS algorithm starting with node 0" in new ContextDirected {
        assert(graph.depthFirstSearch(0) == Set(0,1,2,3))
      }
    }
    "be a int list of all nodes (0,1,2,3)" when {
      "explore the graph with BFS algorithm starting with node 0" in new ContextDirected {
        assert(graph.breadthFirstSearch(0) == Set(0,1,2,3))
      }
    }
    "be a int list of 0,1,2" when {
      "explore the graph `graph2` with DFS algorithm starting with node 0" in new ContextDirected {
        assert(graph2.depthFirstSearch(0) == Set(0,1,2))
      }
    }
    "be a int list of 0,1,2" when {
      "explore the graph `graph2` with BFS algorithm starting with node 0" in new ContextDirected {
        assert(graph2.breadthFirstSearch(0) == Set(0,1,2))
      }
    }
  }

}

trait ContextDirected {

  val graph = AdjacencyListDirectedGraph(List(
    NodeDirected(0, Set(1, 2)),
    NodeDirected(1, Set(0, 3)),
    NodeDirected(2, Set(1)),
    NodeDirected(3, Set(0, 2))
  ))

  val graph2 = AdjacencyListDirectedGraph(List(
    NodeDirected(0, Set(1, 2)),
    NodeDirected(1, Set(0)),
    NodeDirected(2, Set(1)),
    NodeDirected(3, Set(0, 2))
  ))

  val graphInverse = AdjacencyListDirectedGraph(List(
    NodeDirected(0, Set(1, 3)),
    NodeDirected(1, Set(0, 2)),
    NodeDirected(2, Set(0, 3)),
    NodeDirected(3, Set(1))
  ))

  val graphUndirected = AdjacencyListUndirectedGraph(List(
    NodeUndirected(0, Set(1, 2, 3)),
    NodeUndirected(1, Set(0, 2, 3)),
    NodeUndirected(2, Set(0, 1, 3)),
    NodeUndirected(3, Set(0, 1, 2))
  ))

  val graphMatrixList = List(
    List(0, 1, 1, 0),
    List(1, 0, 0, 1),
    List(0, 1, 0, 0),
    List(1, 0, 1, 0)
  )

  val graphMatrixMap: Map[Int, Map[Int, Int]] = Map(
    0 -> Map(0 -> 0, 1 -> 1, 2 -> 1, 3 -> 0),
    1 -> Map(0 -> 1, 1 -> 0, 2 -> 0, 3 -> 1),
    2 -> Map(0 -> 0, 1 -> 1, 2 -> 0, 3 -> 0),
    3 -> Map(0 -> 1, 1 -> 0, 2 -> 1, 3 -> 0)
  )

}
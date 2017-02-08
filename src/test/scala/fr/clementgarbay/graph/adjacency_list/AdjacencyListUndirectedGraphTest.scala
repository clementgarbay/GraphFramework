package fr.clementgarbay.graph.adjacency_list

import fr.clementgarbay.graph.NodeUndirected
import org.scalatest._

/**
  * @author Clément Garbay
  */
class AdjacencyListUndirectedGraphTest extends WordSpec {

  "An undirected adjacency list graph with 4 nodes and 4 edges" should {
    "have 4 nodes" in new ContextUndirected {
      assert(graph.nbNodes == 4)
    }
    "have 4 edges" in new ContextUndirected {
      assert(graph.nbEdges == 4)
    }
    "return true" when {
      "isEdge is called and edge exists" in new ContextUndirected {
        assert(graph.isEdge(3,1))
      }
    }
    "return false" when {
      "isEdge is called and edge doesn't exist" in new ContextUndirected {
        assert(!graph.isEdge(4,3))
      }
    }
    "have 5 edges" when {
      "addEdge is called with a new edge" in new ContextUndirected {
        assert(graph.addEdge(3,2).nbEdges == 5)
      }
    }
    "have 3 edges" when {
      "removeEdge is called" in new ContextUndirected {
        assert(graph.removeEdge(1,2).nbEdges == 3)
      }
    }
    "be a int list of 0,2,3" when {
      "getNeighbors is called with 1 in parameter" in new ContextUndirected {
        assert(graph.getNeighborsIds(1) == Set(0,2,3))
      }
    }
    "be an empty list if node does not exist" when {
      "getNeighbours is called with 12 in parameter" in new ContextUndirected {
        assert(graph.getNeighbors(12) == Set.empty)
      }
    }
    "be the correct adjacency matrix representation (List of List)" when {
      "toAdjacencyMatrix is called" in new ContextUndirected {
        assert(graph.toAdjacencyMatrix == graphMatrixMap)
      }
    }
    "initialize an undirected adjacency list graph" when {
      "from adjacency matrix representation" in new ContextUndirected {
        assert(AdjacencyListUndirectedGraph(graphMatrixList) == graph)
      }
    }
    "be a int list of all nodes (0,1,2,3)" when {
      "explore the graph with DFS algorithm starting with node 0" in new ContextUndirected {
        assert(graph.depthFirstSearch(0) == Set(0,1,2,3))
      }
    }
    "be a int list of all nodes (0,1,2,3)" when {
      "explore the graph with BFS algorithm starting with node 0" in new ContextUndirected {
        assert(graph.breadthFirstSearch(0) == Set(0,1,2,3))
      }
    }
  }

}

trait ContextUndirected {
  val n1 = NodeUndirected(0, Set(1, 2))
  val n2 = NodeUndirected(1, Set(0, 2, 3))
  val n3 = NodeUndirected(2, Set(0, 1))
  val n4 = NodeUndirected(3, Set(1))

  val graph = AdjacencyListUndirectedGraph(List(n1, n2, n3, n4))

  val graphMatrixList = List(
    List(0, 1, 1, 0),
    List(1, 0, 1, 1),
    List(1, 1, 0, 0),
    List(0, 1, 0, 0)
  )

  val graphMatrixMap: Map[Int, Map[Int, Int]] = Map(
    0 -> Map(0 -> 0, 1 -> 1, 2 -> 1, 3 -> 0),
    1 -> Map(0 -> 1, 1 -> 0, 2 -> 1, 3 -> 1),
    2 -> Map(0 -> 1, 1 -> 1, 2 -> 0, 3 -> 0),
    3 -> Map(0 -> 0, 1 -> 1, 2 -> 0, 3 -> 0)
  )

}
package fr.clementgarbay.graph.adjacency_matrix

import fr.clementgarbay.graph.adjacency_list.{AdjacencyListUndirectedGraph, ContextUndirected}
import org.scalatest._

/**
  * @author Clément Garbay
  */
class AdjacencyMatrixUndirectedGraphTest extends WordSpec {

  "An undirected adjacency matrix graph with 4 nodes and 4 edges" should {
    "have 4 nodes" in new ContextMatrixUndirected {
      assert(graph.nbNodes == 4)
    }
    "have 4 edges" in new ContextMatrixUndirected {
      assert(graph.nbEdges == 4)
    }
    "return true" when {
      "isEdge is called and edge exists" in new ContextMatrixUndirected {
        assert(graph.isEdge(3, 1))
      }
    }
    "return true for the other direction" when {
      "isEdge is called and edge exists" in new ContextMatrixUndirected {
        assert(graph.isEdge(1, 3))
      }
    }
    "return false" when {
      "isEdge is called and edge doesn't exist" in new ContextMatrixUndirected {
        assert(!graph.isEdge(4, 3))
      }
    }
    "have 5 edges" when {
      "addEdge is called with a new edge" in new ContextMatrixUndirected {
        assert(graph.addEdge(3, 2).nbEdges == 5)
      }
    }
    "have 3 edges" when {
      "removeEdge is called" in new ContextMatrixUndirected {
        assert(graph.removeEdge(1, 2).nbEdges == 3)
      }
    }
    "be a int list of 0,2,3" when {
      "getNeighbours is called with 1 in parameter" in new ContextMatrixUndirected {
        assert(graph.getNeighborsIds(1) == Set(0, 2, 3))
      }
    }
    "be an empty list if node does not exist" when {
      "getNeighbours is called with 12 in parameter" in new ContextMatrixUndirected {
        assert(graph.getNeighbors(12) == Set.empty)
      }
    }
    "be the correct adjacency matrix representation (List of List)" when {
      "toAdjacencyMatrix is called" in new ContextMatrixUndirected {
        assert(graph.toAdjacencyMatrix == graphMatrix)
      }
    }
    "initialize an undirected adjacency matrix graph" when {
      "from adjacency list representation" in new ContextMatrixUndirected {
        assert(adjacencyMatrix == graph)
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

trait ContextMatrixUndirected {

  val graphMatrix = List(
    List(0, 1, 1, 0),
    List(1, 0, 1, 1),
    List(1, 1, 0, 0),
    List(0, 1, 0, 0)
  )

  val adjacencyList = Map(
    (0, Set(1, 2)),
    (1, Set(0, 2, 3)),
    (2, Set(0, 1)),
    (3, Set(1))
  )

  val graph = AdjacencyMatrixUndirectedGraph(graphMatrix)

  val adjacencyListUndirectedGraph: AdjacencyListUndirectedGraph[Int] = AdjacencyListUndirectedGraph(adjacencyList)
  var adjacencyMatrix: AdjacencyMatrixUndirectedGraph = AdjacencyMatrixUndirectedGraph(adjacencyListUndirectedGraph)

}
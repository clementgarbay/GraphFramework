package fr.clementgarbay.graph

import fr.clementgarbay.graph.adjacency_list.{AdjacencyListDirectedGraph, AdjacencyListUndirectedGraph}
import org.scalatest.WordSpec

/**
  * @author Clément Garbay
  */
class IUndirectedGraphTest extends WordSpec {

  "The prims method" should {
    "Return the right path for the Minimal Spanning Tree" in new ContextUndirectedGraph {
      assert(graph.prim("A") == Set(Edge("A", "B", 5.0), Edge("B", "C", 4.0), Edge("C", "D", 5.0)))
    }
  }

  "The prims method" should {
    "Return the right path for the Minimal Spanning Tree for a more complex one" in new ContextUndirectedGraph {
      assert(graph2.prim(0) == grap2PrimResult)
    }
  }

  "The diameter" should {
    "Return the right number for the cow grah" in new ContextUndirectedGraph {
      assert(graphForDiameter.diameter == 3.0)
    }
  }
}

trait ContextUndirectedGraph {
  val graph: AdjacencyListUndirectedGraph[String] = AdjacencyListDirectedGraph.fromMapWithDistances(Map(
    "B" -> Set(("A", 5.0), ("C", 4.0), ("D", 11.0)),
    "A" -> Set(("B", 5.0), ("C", 10.0)),
    "C" -> Set(("B", 4.0), ("A", 10.0), ("D", 5.0)),
    "D" -> Set(("B", 11.0), ("C", 5.0))
  ))


  val graph2: AdjacencyListUndirectedGraph[Int] = AdjacencyListUndirectedGraph.fromMapWithDistances(Map(
    0 -> Set((1, 4.0), (7, 8.0)),
    1 -> Set((0, 4.0), (7, 11.0), (2, 8.0)),
    2 -> Set((1, 8.0), (8, 2.0), (3, 7.0), (5, 4.0)),
    3 -> Set((2, 7.0), (4, 9.0), (5, 14.0)),
    4 -> Set((3, 9.0), (5, 10.0)),
    5 -> Set((2, 4.0), (3, 14.0), (4, 10.0), (6, 2.0)),
    6 -> Set((5, 2.0), (7, 1.0), (8, 6.0)),
    7 -> Set((0, 8.0), (8, 7.0), (6, 1.0)),
    8 -> Set((2, 2.0), (6, 6.0), (7, 7.0))
  ))

  val grap2PrimResult: Set[Edge[Int]] = Set(
    Edge(0, 1, 4.0),
    Edge(0, 7, 8.0),
    Edge(7, 6, 1.0),
    Edge(6, 5, 2.0),
    Edge(5, 2, 4.0),
    Edge(2, 8, 2.0),
    Edge(2, 3, 7.0),
    Edge(3, 4, 9.0)
  )

  val graphForDiameter: AdjacencyListUndirectedGraph[Int] = AdjacencyListUndirectedGraph(Map(
    0 -> Set(1),
    1 -> Set(0, 2, 3),
    2 -> Set(1, 3),
    3 -> Set(1, 2, 4),
    4 -> Set(3)
  ))
}

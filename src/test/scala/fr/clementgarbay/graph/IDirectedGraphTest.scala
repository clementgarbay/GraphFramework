package fr.clementgarbay.graph

import fr.clementgarbay.graph.adjacency_list.AdjacencyListDirectedGraph
import org.scalatest.WordSpec

/**
  * @author Clément Garbay
  */
class IDirectedGraphTest extends WordSpec {

  "The getStronglyConnectedComponents method" should {
    "right compute the strongly connected components from the node 2" in new ContextDirectedGraph {
      assert(graph.getStronglyConnectedComponents(2) == Set(Set(12, 11), Set(5, 1, 2, 3, 4), Set(6), Set(9, 7, 8), Set(10)))
    }
  }

  "The getShortestPathWithBellmanFord method" should {
    "right compute the shortest distances and parents from the node `3`" in new ContextDirectedGraph {
      assert(
        graphWithDistances.getShortestPathWithBellmanFord("3") ==
        List(
          Path(3.0, List("3", "4")),
          Path(5.0, List("3", "0", "1")),
          Path(4.0, List("3", "0")),
          Path(14.0, List("3", "0", "2"))
        )
      )
    }
  }

  "The getShortestPathWithDijkstra method" should {
    "right compute the shortest path from the node `a` to `e`" in new ContextDirectedGraph {
      assert(
        graphWithDistances2.getShortestPathWithDijkstra("a", "e") ==
        Path(26.0, List("a", "c", "d", "e"))
      )
    }
  }

}

trait ContextDirectedGraph {

  val graph: AdjacencyListDirectedGraph[Int] = AdjacencyListDirectedGraph(List(
    NodeDirected(1, Set(4)),
    NodeDirected(2, Set(1, 5, 7)),
    NodeDirected(3, Set(2)),
    NodeDirected(4, Set(3, 5)),
    NodeDirected(5, Set(3, 6)),
    NodeDirected(6, Set(10)),
    NodeDirected(7, Set(6, 8)),
    NodeDirected(8, Set(9)),
    NodeDirected(9, Set(7, 10)),
    NodeDirected(10, Set.empty[Int]),
    NodeDirected(11, Set(10, 12)),
    NodeDirected(12, Set(8, 11))
  ))

  val graphWithDistances: AdjacencyListDirectedGraph[String] =
    AdjacencyListDirectedGraph.fromMapWithDistances(Map(
      "0" -> Set(("1", 1.0), ("2", 10.0)),
      "1" -> Set(("3", 3.0)),
      "2" -> Set(("3", -10.0)),
      "3" -> Set(("0", 4.0), ("4", 3.0)),
      "4" -> Set.empty[(String, Double)]
    ))

  var graphWithDistances2: AdjacencyListDirectedGraph[String] =
    AdjacencyListDirectedGraph.fromMapWithDistances(Map(
      "a" -> Set(("b", 7.0), ("c", 9.0), ("f", 14.0)),
      "b" -> Set(("c", 10.0), ("d", 15.0)),
      "c" -> Set(("d", 11.0), ("f", 2.0)),
      "d" -> Set(("e", 6.0)),
      "e" -> Set(("f", 9.0)),
      "f" -> Set.empty[(String, Double)]
    ))

}

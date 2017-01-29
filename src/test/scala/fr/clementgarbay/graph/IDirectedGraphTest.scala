package fr.clementgarbay.graph

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
        (
          Map("0" -> 4.0, "1" -> 5.0, "2" -> 14.0, "3" -> 0.0, "4" -> 3.0),
          List(
            Path("3","0",List(Arc("3","0",4.0))),
            Path("3","1",List(Arc("3","0",4.0), Arc("0","1",1.0))),
            Path("3","2",List(Arc("3","0",4.0), Arc("0","2",10.0))),
            Path("3","4",List(Arc("3","4",3.0)))
          )
        )
      )
    }
  }

}

trait ContextDirectedGraph {

  val graph = AdjacencyListDirectedGraph(List(
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

  val graphWithDistances = AdjacencyListDirectedGraph(List(
    NodeDirected("0", Set(SemiArc("1", 1.0), SemiArc("2", 10.0))),
    NodeDirected("1", Set(SemiArc("3", 3.0))),
    NodeDirected("2", Set(SemiArc("3", -10.0))),
    NodeDirected("3", Set(SemiArc("0", 4.0), SemiArc("4", 3.0))),
    NodeDirected("4", Set.empty[SemiArc[String]])
  ))

}

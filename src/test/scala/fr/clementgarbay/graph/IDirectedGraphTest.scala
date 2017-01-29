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
    NodeDirected(12, Set(8, 11)))
  )

}

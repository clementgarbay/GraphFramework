package fr.clementgarbay.graph

/**
  * @author Clément Garbay
  */
object Main extends App {

  override def main(args: Array[String]): Unit = {
    val n1 = NodeUndirected(1, List.empty)
    val n2 = NodeUndirected(2, List(n1))
    val n3 = NodeUndirected(3, List(n1,n2))
    val n4 = NodeUndirected(4, List(n3))

    val graph = AdjacencyListUndirectedGraph(List(n1,n2,n3,n4))

    print(graph.removeEdge(2,1))
  }
}

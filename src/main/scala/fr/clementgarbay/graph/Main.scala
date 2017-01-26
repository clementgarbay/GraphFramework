package fr.clementgarbay.graph

/**
  * @author Clément Garbay
  */
object Main extends App {

  override def main(args: Array[String]): Unit = {

//    val graph = GraphTools.generateUndirectedGraph(10,10)

//    println(graph.depthFirstSearch(0))

    val adjacencyList = AdjacencyListDirectedGraph(Set(
    NodeDirected(1, Set(4)),
    NodeDirected(2, Set(1, 5, 7)),
    NodeDirected(3, Set(2)),
    NodeDirected(4, Set(3, 5)),
    NodeDirected(5, Set(3, 6)),
    NodeDirected(6, Set(10)),
    NodeDirected(7, Set(6, 8)),
    NodeDirected(8, Set(9)),
    NodeDirected(9, Set(7, 10)),
    NodeDirected(10, Set()),
    NodeDirected(11, Set(10, 12)),
    NodeDirected(12, Set(8, 11))))

//    adjacencyList.baseDfs(1)

    adjacencyList.computeStrongConnectivity(2)
    println("coucou")



//    GraphTools.displayGraph(graph)
//
//    println(graph.getNbEdges)
//    println(graph.isEdge(1,14))
//
//    GraphTools.displayGraph(graph.removeEdge(0,1))

//    val graph2 = GraphTools.generateDirectedGraph(3,2)

//    GraphTools.displayGraph(graph)
//    GraphTools.displayGraph(graph.toAdjacencyMatrix)

//    GraphTools.displayGraph(graph2)




//    GraphTools.displayGraph(graph2.computeInverse)


    //
//    val n1 = NodeUndirected(1, List.empty)
//    val n2 = NodeUndirected(2, List(n1))
//    val n3 = NodeUndirected(3, List(n1,n2))
//    val n4 = NodeUndirected(4, List(n3))
//
//    val graph = AdjacencyListUndirectedGraph(List(n1,n2,n3,n4))
//
//    print(graph.removeEdge(2,1))
  }
}

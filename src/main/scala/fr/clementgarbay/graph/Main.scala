package fr.clementgarbay.graph

import fr.clementgarbay.graph.Link._
/**
  * @author Clément Garbay
  */
object Main extends App {

  override def main(args: Array[String]): Unit = {
    val link: Arc[Int] = Arc(1, 2, 4.0)
    val link2: Arc[Int] = Arc(3, 4, 10.0)
    val link3: Arc[Int] = Arc(3, 4, 6.0)
    val link4: Arc[Int] = Arc(3, 4, 12.0)
    val link5: Arc[Int] = Arc(3, 4, 14.0)
    val link6: Arc[Int] = Arc(3, 4, 9.0)

    println(link2 < link3)

    val binaryHeap = BinaryHeap(List(4,10,6,12,14,9))

    println(binaryHeap)
    println(binaryHeap.add(1))
    println(binaryHeap.removeRoot())

    val binaryHeapArcs = BinaryHeap[Link[Int]](List(link, link2, link3, link4, link5, link6))
    println(binaryHeapArcs)
    println(binaryHeapArcs.add(Arc(5, 6, 1.0)))
//    val graph1 = GraphTools.generateUndirectedGraph(10,10)


//    val graph = AdjacencyListDirectedGraph(List(
//      NodeDirected(0, Set(1, 2)),
//      NodeDirected(1, Set(0, 3)),
//      NodeDirected(2, Set(1)),
//      NodeDirected(3, Set(0, 2))
//    ))
//
//    print(graph.nbNodes)

//    println(graph.depthFirstSearch(0))


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

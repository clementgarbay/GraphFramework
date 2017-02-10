package fr.clementgarbay.graph

import fr.clementgarbay.graph.Link._
import fr.clementgarbay.graph.adjacency_list.{AdjacencyListDirectedGraph, AdjacencyListUndirectedGraph}
import fr.clementgarbay.graph.heap.BinaryHeap

/**
  * @author Clément Garbay
  * @author Anaël Chardan
  */
object Main extends App {

  override def main(args: Array[String]): Unit = {
    val graph = AdjacencyListUndirectedGraph(Map(
      1 -> Set(2, 3, 4, 7),
      2 -> Set(1, 3, 4, 5, 7),
      3 -> Set(1, 2, 7, 6, 4),
      4 -> Set(2, 1, 3, 6, 5),
      5 -> Set(4, 2, 7, 6),
      6 -> Set(5, 4, 3, 7),
      7 -> Set(1, 2, 3, 5, 6)
    ))

    //L'algorithme utilisé est dans
    val resultColoration = graph.getColororifiedNode

    //CHAQUE Valeur etant les organisation peut s'executer en meme temps
    println(resultColoration)
  }
}

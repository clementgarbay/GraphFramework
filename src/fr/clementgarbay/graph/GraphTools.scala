package fr.clementgarbay.graph

import scala.util.Random

/**
  * @author Clément Garbay
  */
object GraphTools extends App {

  def generateGraphData(n: Int, m: Int, s: Boolean): List[List[Int]] = {

    val coords: List[(Int,Int)] = (1 to m)
      .foldLeft(List.empty[(Int,Int)]) { (acc: List[(Int,Int)], _) =>
        var coor = (Random.nextInt(n), Random.nextInt(n))

        do {
          coor = (Random.nextInt(n), Random.nextInt(n))
        } while (acc.contains(coor) || (coor._1 == coor._2))

        coor :: acc
      }

//    val test: Seq[Int] = {
//      for {
//        j <- 0 to n
//        i <- 0 to n
//        isOne = coords contains ((i,j))
//      } yield {
//        if (isOne) true
//        else false
//      }
//    }

    var list = List.fill(n,n)(0)

    for (coor <- coords) {
      list = list.updated(coor._2, list(coor._2).updated(coor._1, 1))
      if (s) {
        list = list.updated(coor._1, list(coor._1).updated(coor._2, 1))
      }
    }

    list
  }

  def displayGraph(graph: List[List[Int]]) = {
    print(graph.map(_.mkString("  ")).mkString("\n"))
  }

  displayGraph(generateGraphData(10, 2, true))
}

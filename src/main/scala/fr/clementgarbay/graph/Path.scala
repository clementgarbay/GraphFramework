package fr.clementgarbay.graph

/**
  * @author Clément Garbay
  * @author Anaël Chardan
  */
case class Path[T](distance: Double, path: List[T]) {

  lazy val from: T = path.head
  lazy val to: T = path.last
  lazy val reverse: Path[T] = Path(distance, path.reverse)

  def addToTop(dist: Double, elem: T): Path[T] = {
    Path(distance + dist, elem :: path)
  }

  def addToEnd(dist: Double, elem: T): Path[T] = {
    Path(distance + dist, path :+ elem)
  }

}

object Path {
  def empty[T] = Path(0.0, List.empty[T])

  def toArcs[T](path: Path[T]): List[Arc[T]] = {
    path.path.zip(path.path.tail).map(e => Arc(e._1, e._2))
  }
}

package fr.clementgarbay.graph

/**
  * @author Clément Garbay
  *
  * @note By default, the distance is 1.
  */

trait SemiLink[T] {
  val to: T
  val distance: Double
}

trait Link[T] extends SemiLink[T] with Ordered[Link[T]] {
  val from: T
  val reverse: Link[T]

  override def compare(that: Link[T]): Int = distance.compare(that.distance)
}

object Link {
  implicit def linkOrdering[T]: Ordering[Link[T]] = Ordering.fromLessThan(_.distance < _.distance)
}

trait SemiLinkTransformable[T] extends SemiLink[T] {
  def toLink(from: T): Link[T]
}

case class SemiArc[T](to: T, distance: Double = 1.0) extends SemiLinkTransformable[T] {
  override def toLink(from: T): Arc[T] = Arc(from, to, distance)
}

case class Arc[T](from: T, to: T, distance: Double = 1.0) extends Link[T] {
  override lazy val reverse: Link[T] = Arc(to, from, distance)
}

case class SemiEdge[T](to: T, distance: Double = 1.0) extends SemiLinkTransformable[T] {
  override def toLink(from: T): Edge[T] = Edge(from, to, distance)
}

case class Edge[T](from: T, to: T, distance: Double = 1.0) extends Link[T] {
  override lazy val reverse: Link[T] = Edge(to, from, distance)
}
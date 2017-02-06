package fr.clementgarbay.graph
package heap

/**
  * @author Clément Garbay
  * @author Anaël Chardan
  */
case class BinaryHeap[T](values: List[T] = List.empty)(implicit comp: Ordering[T]) {

  val root: Option[T] = values.lift(0)

  def add(value: T): BinaryHeap[T] = {
    BinaryHeap(values :+ value).percolateUp(values.size)
  }

  def removeRoot(): BinaryHeap[T] = {
    BinaryHeap(values.updated(0, values.last).dropRight(1)).percolateDown(0)
  }

  private def percolateUp(index: Int): BinaryHeap[T] = {
    val value = values(index)
    val fatherIndex = (index - 1) / 2
    val fatherValueOpt = values.lift(fatherIndex)

    if (fatherValueOpt.isEmpty || comp.lteq(fatherValueOpt.get, value)) return this

    swap(index, fatherIndex).percolateUp(fatherIndex)
  }

  private def percolateDown(index: Int): BinaryHeap[T] = {
    val indexToPermuteOpt =
      List((2 * index + 1, values.lift(2 * index + 1)), (2 * index + 2, values.lift(2 * index + 2)))
        .collect({
          case (i, iValueOpt) if iValueOpt.isDefined => (i, iValueOpt.get)
        })
        .reduceOption(Ordering.by((_: (Int, T))._2).min)
        .map(_._1)

    indexToPermuteOpt match {
      case Some(indexToPermute: Int) if comp.gt(values(index), values(indexToPermute)) =>
        swap(index, indexToPermute).percolateDown(indexToPermute)
      case _ => this
    }
  }

  private def swap(i1: Int, i2: Int): BinaryHeap[T] = {
    BinaryHeap(values.updated(i1, values(i2)).updated(i2, values(i1)))
  }

}

package fr.clementgarbay.graph

/**
  * @author Clément Garbay
  * @author Anaël Chardan
  */
case class BinaryHeap(values: List[Int]) {

  def add(value: Int): BinaryHeap = {
    BinaryHeap(values :+ value).percolateUp(values.size)
  }

  def removeRoot(): BinaryHeap = {
    BinaryHeap(values.updated(0, values.last).dropRight(1)).percolateDown(0)
  }

  private def percolateUp(index: Int): BinaryHeap = {
    val value = values(index)
    val fatherIndex = (index - 1) / 2
    val fatherValueOpt = values.lift(fatherIndex)

    if (fatherValueOpt.isEmpty || fatherValueOpt.get <= value) return this

    swap(index, fatherIndex).percolateUp(fatherIndex)
  }

  private def percolateDown(index: Int): BinaryHeap = {
    val indexToPermuteOpt =
      List((2 * index + 1, values.lift(2 * index + 1)), (2 * index + 2, values.lift(2 * index + 2)))
        .collect({
          case (i, iValueOpt) if iValueOpt.isDefined => (i, iValueOpt.get)
        })
        .reduceOption(Ordering.by((_: (Int, Int))._2).min)
        .map(_._1)

    indexToPermuteOpt match {
      case Some(indexToPermute: Int) if values(index) > values(indexToPermute) =>
        swap(index, indexToPermute).percolateDown(indexToPermute)
      case _ => this
    }
  }

  private def swap(i1: Int, i2: Int): BinaryHeap = {
    BinaryHeap(values.updated(i1, values(i2)).updated(i2, values(i1)))
  }

}

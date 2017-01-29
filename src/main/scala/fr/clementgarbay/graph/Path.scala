package fr.clementgarbay.graph

/**
  * @author Clément Garbay
  */
case class Path[T](from: T, to: T, path: List[Link[T]]) {

  val distance: Double = path.map(_.distance).sum

}

object Path {

  def getPathFromParents[T](startingNodeId: T, endingNodeId: T, parents: Map[T, Option[SemiLinkTransformable[T]]]): Path[T] = {
    def getPathFromParentsRec(nodeId: T, path: List[Link[T]]): List[Link[T]] = {
      val parentOpt = parents(nodeId)
      if (parentOpt.isEmpty || parentOpt == startingNodeId) path
      else getPathFromParentsRec(parentOpt.get.to, path :+ parentOpt.get.toLink(nodeId).reverse)
    }

    Path(startingNodeId, endingNodeId, getPathFromParentsRec(endingNodeId, List.empty).reverse)
  }

}
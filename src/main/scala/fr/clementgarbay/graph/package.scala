package fr.clementgarbay

/**
  * @author Clément Garbay
  * @author Anaël Chardan
  */
package object graph {

  type Matrix = List[List[Int]]

  class AsInt(b: Boolean) {
    def toInt: Int = if (b) 1 else 0
  }

  implicit def convertBooleanToInt(b: Boolean): AsInt = new AsInt(b)
}
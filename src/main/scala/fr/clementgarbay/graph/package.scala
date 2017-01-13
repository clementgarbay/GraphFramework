package fr.clementgarbay

/**
  * @author Clément Garbay
  */
package object graph {

  class AsInt(b: Boolean) {
    def toInt: Int = if (b) 1 else 0
  }

  implicit def convertBooleanToInt(b: Boolean): AsInt = new AsInt(b)
}
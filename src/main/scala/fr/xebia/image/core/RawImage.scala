package fr.xebia.image.core

import scala.annotation.tailrec

case class Position(x: Int, y: Int) extends Ordered[Position] {
  override def toString: String = s"($x,$y)"

  import scala.math.Ordered.orderingToOrdered
  override def compare(that: Position): Int = (this.x, this.y) compare (that.x, that.y)
}

/**
  * Generic image, as a list of list of pixels.
  *
  * @param content list of list of pixels.
  * @tparam U the type of the pixel value.
  */
case class RawImage[U](content: List[List[U]]) {
  require(content.nonEmpty, "Empty image")
  require(content.map(_.size).distinct.size == 1, "Not all rows have the same size")

  val width = content.head.size

  val height = content.size

  /**
    * TODO 01: Get the first element that matches following a left-right / up-down strategy
    * Note:
    *  - get row numbers using <code>content.indices</code> zipped with the content
    *  - 'find' the row containing the searched element. When using tuples you must use
    *    <code>case (first, second)=>...</code> in your lambdas
    *  - 'map' the result into a Position instance
    */
  def firstThatMatches(searched: U): Option[Position] = {
    val zipped: List[(Int, List[U])] = ???
    ???
  }

  /**
    * TODO 02 - Replace the pixels at the specified position by the specified pixel value.
    * @param neighborList the position where pixel value must be replaced
    * @param value the new pixel value to erase the specified neighborList with
    * @return an updated image with specified pixels replaced by specified value.
    * Note:
    *  - recursively go through the specified position and call function 'replaceValueInContent'
    *   to replace the current content
    */
  def replace(neighborList: List[Position], value: U): RawImage[U] = {
    //@tailrec TODO: uncomment this once implemented
    def go(updatedImage: RawImage[U], remainingNeighbor: List[Position]): RawImage[U] = {
      ???
    }
    go(this, neighborList)
  }

  // Replace the value in the specified position into the image
  private def replaceValueInContent(position: Position, currentImage: RawImage[U], newValue: U): List[List[U]] = {
    val valueReplaced = currentImage.content(position.y).updated(position.x, newValue)
    currentImage.content.updated(
      position.y,
      valueReplaced
    )
  }

  /**
    * TODO 03
    * @param center searched position
    * @return all the neighbors pixels of the searched position
    * Note:
    *   - filter out positions out of bounds
    */
  def neighborsOnly(center: Position): List[Position] = {
    val neigh = scala.collection.mutable.ArrayBuffer.empty[Position]
    ???
  }

  /**
    * @param center searched position
    * @return the position of the specified <code>center</code> and all its neighbors pixels
    */
  def neighborsAndSelf(center: Position): List[Position] =
    neighborsOnly(center) :+ center

  def at(pos: Position): U = content(pos.y)(pos.x)

}

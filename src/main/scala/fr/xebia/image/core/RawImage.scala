package fr.xebia.image.core

import scala.annotation.tailrec

/**
  * The wheel reivented for the sake of the exercise
  */
case class Position(x: Int, y: Int) {
  override def toString: String = s"($x,$y)"
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
    * Get the first element that matches following a left-right / up-down strategy
    */
  def firstThatMatches(searched: U): Option[Position] = {
    val zipped: List[(Int, List[U])] = content.indices
      .toList
      .zip(content)
    zipped
      .find { case (y, xValues) => xValues.contains(searched) }
      .map { case (y, xValues) => Position(xValues.indexOf(searched), y) }
  }

  private def takeWhile(predicate: U => Boolean): List[U] =
    content.collect { case (row) => row.filter(predicate(_)) }.flatten

  /**
    * Replace the pixels at the specified position by the specified pixel value.
    * @param neighborList the position where pixel value must be replaced
    * @param value the new pixel value to erase the specified neighborList with
    * @return an updated image with specified pixels replaced by specified value.
    */
  def replace(neighborList: List[Position], value: U): RawImage[U] = {
    @tailrec
    def go(updatedImage: RawImage[U], remainingNeighbor: List[Position]): RawImage[U] = {
      remainingNeighbor match {
        case Nil =>
          updatedImage

        case currentPos :: remainingPositions =>
          go(
            copy(content = replaceValueInContent(currentPos, updatedImage, value)),
            remainingPositions
          )
      }
    }
    go(this, neighborList)
  }

  private def replaceValueInContent(position: Position, currentImage: RawImage[U], newValue: U): List[List[U]] = {
    val valueReplaced = currentImage.content(position.y).updated(position.x, newValue)
    currentImage.content.updated(
      position.y,
      valueReplaced
    )
  }

  def neighborsOnly(center: Position): List[Position] = {
    val neigh = scala.collection.mutable.ArrayBuffer.empty[Position]
    neigh += center.copy(x = center.x - 1, y = center.y - 1)
    neigh += center.copy(x = center.x - 1)
    neigh += center.copy(x = center.x - 1, y = center.y + 1)
    neigh += center.copy(y = center.y + 1)
    neigh += center.copy(y = center.y + 1, x = center.x + 1)
    neigh += center.copy(y = center.y - 1)
    neigh += center.copy(y = center.y - 1, x = center.x + 1)
    neigh += center.copy(x = center.x + 1)

    neigh
      .toList
      .filter(pos => pos.x >= 0 && pos.y >= 0)
      .filter(pos => pos.x < width && pos.y < height)
  }

  /**
    * @param center
    * @return the position of the specified <code>center</code> and all its neighbors pixels
    */
  def neighborsAndSelf(center: Position): List[Position] =
    neighborsOnly(center) :+ center

  def at(pos: Position): U = content(pos.y)(pos.x)

}

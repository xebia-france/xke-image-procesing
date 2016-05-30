package fr.xebia.image.core

import fr.xebia.image.export.{AlwaysWhite, ImageWriter}

import scala.annotation.tailrec
import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global

/**
  * A functor to process images. It has operations dedicated to the detection of connected elements.
  *
  * @param rawImage
  * @tparam U the type of the pixel value
  */
case class ImageProcessingFunctor[U](rawImage: RawImage[U]) {

  /**
    * TODO 05
    * Count the number of components in this processed image and replaces all connected pixels in those components by an "empty pixel" marker.
    * Example: if <code>contentValue</code is '@', the following image has a connected elements count of 2. There is 2 distinguishable objects composed of connected '@'.
    * <pre>
    * ..........
    * ..@@..@@@.
    * ..@@...@..
    * ..@@...@..
    * ..........
    * </pre>
    *
    * @param contentValue the value that connected pixels must match
    * @param emptyValue   the pixel value that will mark processed pixels
    * @return the number of components in this processed image (a component is a set of connected pixels).
    * Note:
    *   - Find the first position that matches the specified value
    *   - Recursively 'go' through the image until no more position matches the specified value :
    *     - Each time you find a position that matches the specified value, you must find all the neighbors withe the same value ( = the connected points)
    *       by propagating a front from the first position
    *     - One you found all connected points, replace them by the specified empty value, and restart withe the modified image.
    */
  def countConnectedElements(contentValue: U, emptyValue: U): Int = {
    @tailrec
    def go(copyImage: RawImage[U], maybePosition: Option[Position], connectedElements: Int): Int = {
      maybePosition match {
        case None =>
          ???

        case Some(seed) =>
          val connectedPoints = ???
          val newImage = ???
          go(newImage, getFirstThatMatchesOn(newImage, contentValue), connectedElements + 1)
      }
    }
    go(this.rawImage, firstThatMatches(contentValue), 0)
  }

  /*
   * TODO 04
    * Helper function to find all connected pixels with the specified value.
    * @param seeds a list of position to start the lookup. Positions must be neighbors in order to find only connected pixels.
    * @param searchedValue the value that all connected pixels must match
    * @param markWith the "empty pixel" marker that will replace pixels once processed.
    * @return the list of positions of the connected pixels (i.e. belonging to a single component)
    * Note :
    *   - filter neighbors in imageCopy having the searched value
    *   - recursively get the neighborsAndSelf from the current position and replace those positions with the 'mark'
    *   - don't forget to concat the remaining seed with the neighborhood of the current position for the next iteration
    */
  private[image] def propagateFront(seeds: List[Position], searchedValue: U, markWith: U): List[Position] = {
    //@tailrec uncomment this once implemented
    def go(imageCopy: RawImage[U], neighbors: List[Position], positions: List[Position]): List[Position] = {
      ???
    }
    go(rawImage, seeds, List.empty[Position])
  }

  /**
    * Replace pixels that match the specified predicate.
    *
    * @param predicate the predicate
    * @param replaceBy the pixel value that will replace pixels matching predicate <code>p</code>
    * @return a new processing functor where pixels fulfilling <code>p</code> have been replaced by <code>replaceBy</code>
    */
  def threshold(predicate: U => Boolean, replaceBy: U): ImageProcessingFunctor[U] =
    map[U](cell => if (predicate(cell)) replaceBy else cell)

  /**
    * Change image
    *
    * @param f the mapping function (converts a pixel of one type to another)
    * @tparam R the type of the pixel value in the resulting image
    * @return the processing functor for the converted image
    */
  def map[R](f: U => R): ImageProcessingFunctor[R] = {
    new ImageProcessingFunctor[R](
      RawImage(
        rawImage.content.map(_.map(cell => f(cell)))
      )
    )
  }

  /**
    * @param center the position of the target pixel
    * @return a list of #Position containing the positions of target pixels and all its neighbors
    */
  def neighborsAndSelf(center: Position): List[Position] = rawImage.neighborsAndSelf(center)

  /**
    * @param searched a specific pixel value that we are looking for
    * @return the position of the first pixel from this image processing matching the given pixel value or None if not found
    */
  def firstThatMatches(searched: U): Option[Position] =
    getFirstThatMatchesOn(rawImage, searched)

  protected def getFirstThatMatchesOn(onImage: RawImage[U], searched: U): Option[Position] =
    onImage.firstThatMatches(searched)

  /**
    * Return a new image processing where pixels at the specified positions have been replaced by
    * the specified pixel value.
    *
    * @param neighborList list of target position of pixels to be replaced
    * @param value        the pixel value that will replace the specified positions values
    * @return a new image processing where pixels at the specified positions have the specified value
    */
  def replace(neighborList: List[Position], value: U): ImageProcessingFunctor[U] =
    new ImageProcessingFunctor(rawImage.replace(neighborList, value))

  /**
    * @param pos the requested position
    * @return the pixel value at the requested position
    */
  def at(pos: Position): U = rawImage.at(pos)

}

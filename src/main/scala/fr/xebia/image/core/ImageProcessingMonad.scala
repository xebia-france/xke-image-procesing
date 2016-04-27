package fr.xebia.image.core

import scala.annotation.tailrec

/**
  * The base trait for the image processing monad.
  * @tparam U the type of the pixel value
  */
trait BaseImageTools[U] {

  val rawImage: RawImage[U]

  /**
    * @param center the position of the target pixel
    * @return a list of #Position containing the positions of target pixels and all its neighbors
    */
  def neighborsAndSelf(center: Position): List[Position] = rawImage.neighborsAndSelf(center)

  /**
    * @param searched a specific pixel value that we are looking for
    * @return the position of the first pixel from this image processing matching the given pixel value or None if not found
    */
  def getFirstThatMatches(searched: U): Option[Position] =
    getFirstThatMatchesOn(rawImage, searched)

  protected def getFirstThatMatchesOn(onImage: RawImage[U], searched: U): Option[Position] =
    onImage.firstThatMatches(searched)

  /**
    * Return a new image processing where pixels at the specified positions have been replaced by
    * the specified pixel value.
    * @param neighborList list of target position of pixels to be replaced
    * @param value the pixel value that will replace the specified positions values
    * @return a new image processing where pixels at the specified positions have the specified value
    */
  def replace(neighborList: List[Position], value: U): ImageProcessingMonad[U] =
    new ImageProcessingMonad(rawImage.replace(neighborList, value))

  /**
    * @param pos the requested position
    * @return the pixel value at the requested position
    */
  def at(pos: Position): U = rawImage.at(pos)

}

/**
  * A monad to process images. It extends {@link BaseImageTools} with operations dedicated to the detection of connected elements.
  * @param rawImage
  * @tparam U the type of the pixel value
  */
case class ImageProcessingMonad[U](rawImage: RawImage[U]) extends BaseImageTools[U] {

  /**
    * Count the number of components in this processed image and replaces all connected pixels in those components by an "empty pixel" marker.
    * Example: if <code>contentValue</code is '@', the following image has a connected elements count of 2. There is 2 distinguishable objects composed of connected '@'.
    * <pre>
    * ..........
    * ..@@..@@@.
    * ..@@...@..
    * ..@@...@..
    * ..........
    * </pre>
    * @param contentValue the value that connected pixels must match
    * @param emptyValue  the pixel value that will mark processed pixels
    * @return the number of components in this processed image (a component is a set of connected pixels).
    */
  def countConnectedElements(contentValue: U, emptyValue: U): Int = {
    @tailrec
    def go(copyImage: RawImage[U], maybePosition: Option[Position], connectedElements: Int): Int = {
      maybePosition match {
        case None =>
          connectedElements

        case Some(seed) =>
          val connectedPoints = propagateFront(
            copyImage.neighborsAndSelf(seed),
            contentValue,
            emptyValue
          )
          val newImage = copyImage.replace(connectedPoints, emptyValue)
          go(newImage, getFirstThatMatchesOn(newImage, contentValue), connectedElements + 1)
      }
    }
    go(this.rawImage, getFirstThatMatches(contentValue), 0)
  }

  /*
    * Helper function to find all connected pixels with the specified value.
    * @param seeds a list of position to start the lookup. Positions must be neighbors in order to find only connected pixels.
    * @param searchedValue the value that all connected pixels must match
    * @param markWith the "empty pixel" marker that will replace pixels once processed.
    * @return the list of positions of the connected pixels (i.e. belonging to a single component)
    */
  private[image] def propagateFront(seeds: List[Position], searchedValue: U, markWith: U): List[Position] = {
    @tailrec
    def go(imageCopy: RawImage[U], neighbors: List[Position], positions: List[Position]): List[Position] = {
      neighbors.filter(imageCopy.at(_) == searchedValue) match {
        case Nil =>
          positions
        case currentSeed :: remainingSeed =>
          val newNeighborhood: List[Position] = imageCopy.neighborsAndSelf(currentSeed)
          val newImage = imageCopy.replace(List(currentSeed), markWith)
          go(newImage, remainingSeed ++ newNeighborhood, positions :+ currentSeed)
      }
    }
    go(rawImage, seeds, List.empty[Position])
  }

  /**
    * Replace pixels that match the specified predicate.
    *
    * @param p the predicate
    * @param replaceBy the pixel value that will replace pixels matching predicate <code>p</code>
    * @return a new processing monad where pixels fulfilling <code>p</code> have been replaced by <code>replaceBy</code>
    */
  def threshold(p: U => Boolean, replaceBy: U): ImageProcessingMonad[U] =
    map[U](cell => if (p(cell)) replaceBy else cell)

  /**
    * Change image
    * @param f the mapping function (converts a pixel of one type to another)
    * @tparam R the type of the pixel value in the resulting image
    * @return the processing monad for the converted image
    */
  def map[R](f: U => R): ImageProcessingMonad[R] = {
    new ImageProcessingMonad[R](
      RawImage(
        rawImage.content.map(_.map(cell => f(cell)))
      )
    )
  }

}

package fr.xebia.image

import scala.annotation.tailrec

case class ImageProcessingMonad[U](rawImage: RawImage[U], emptyValue: U) {

  def countConnectedElements(contentValue: U): Int = {
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

  def getFirstThatMatches(searched: U): Option[Position] =
    getFirstThatMatchesOn(rawImage, searched)

  private def getFirstThatMatchesOn(onImage: RawImage[U], searched: U): Option[Position] =
    onImage.getFirstThatMatches(searched)

  def replace(neighborList: List[Position], value: U): ImageProcessingMonad[U] =
    ImageProcessingMonad(rawImage.replace(neighborList, value), emptyValue)

  def threshold(f: U => Boolean, replaceBy: U): ImageProcessingMonad[U] =
    map[U](cell => if (f(cell)) replaceBy else cell, emptyValue)

  def map[R](f: U => R, defaultEmptyValue: R): ImageProcessingMonad[R] = {
    ImageProcessingMonad[R](
      RawImage(
        rawImage.content.map(_.map(cell => f(cell)))
      ),
      defaultEmptyValue
    )
  }
}


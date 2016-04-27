package fr.xebia.image

import fr.xebia.image.core.{ImageProcessingFunctor, Position, RawImage}
import fr.xebia.image.export.ImageWriter

object TestFactory {

  case class ImageBuilder() {
    def filledWith(aContent: List[List[String]]) = RawImage[String](aContent)
  }

  def aStringImage = ImageBuilder()

  def anImageFunctorFrom(rawContent: String): ImageProcessingFunctor[String] = {
    val contents: List[List[String]] = rawContent
      .split("\n")
      .map(_.toCharArray.toList.map(_.toString))
      .toList
      .filter(_.map(_.trim).mkString.nonEmpty)
    ImageProcessingFunctor(RawImage(contents))
  }

  object ImagingTools {

    def aSeedThatMatches(processingMonad: ImageProcessingFunctor[String], position: Position, expectedValue: String): Position = {
      val firstSeed = processingMonad
        .firstThatMatches("#")
        .getOrElse(throw new IllegalStateException("# not found"))
      assert(firstSeed == position)
      assert(processingMonad.at(firstSeed) == "#")
      firstSeed
    }

    def writeToFile(processingMonad: ImageProcessingFunctor[String], front: List[Position], fileName: String, newContent: String = "@"): Unit =
      ImageWriter.writeToFile(fileName, processingMonad.replace(front, newContent).rawImage)

  }

}

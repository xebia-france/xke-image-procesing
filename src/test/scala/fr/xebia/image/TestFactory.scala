package fr.xebia.image

object TestFactory {

  case class ImageBuilder() {
    def filledWith(aContent: List[List[String]]) = RawImage[String](aContent)
  }

  def aStringImage = ImageBuilder()

  def anImageWrapperFrom(rawContent: String): ImageProcessingMonad[String] = {
    val contents: List[List[String]] = rawContent
      .split("\n")
      .map(_.toCharArray.toList.map(_.toString))
      .toList
      .filter(_.map(_.trim).mkString.nonEmpty)
    ImageProcessingMonad(RawImage(contents))
  }

  object ImagingTools {

    def aSeedThatMatches(processingMonad: ImageProcessingMonad[String], position: Position, expectedValue: String): Position = {
      val firstSeed = processingMonad
        .getFirstThatMatches("#")
        .getOrElse(throw new IllegalStateException("# not found"))
      assert(firstSeed == position)
      assert(processingMonad.at(firstSeed) == "#")
      firstSeed
    }

    def writeToFile(processingMonad: ImageProcessingMonad[String], front: List[Position], fileName: String, newContent: String = "@"): Unit =
      ImageWriter.writeToFile(fileName, processingMonad.replace(front, newContent).rawImage)

  }

}

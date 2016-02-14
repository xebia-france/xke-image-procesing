package fr.xebia.image

object ImageProcessing extends App {

  private val rawImage = ImageBuilder.fromFile("/input.O.txt")
  val service = ImageProcessingMonad[String](rawImage)
  service.countUnconnectedElements("#", "@")

  private val segmentedImg = service.threshold(cell => cell == "#", "@")
  segmentedImg.rawImage.writeToFile("segmented.txt")

}

package fr.xebia.image

object ImageProcessing extends App {

  private val rawImage = ImageBuilder.fromFile("/input.O.txt")
  val service = ImageProcessingMonad[String](rawImage)
  service.countConnectedElements("#", ".")

  private val segmentedImg = service.threshold(cell => cell == "#", "@")
  segmentedImg.rawImage.writeToFile("segmented.txt")

}

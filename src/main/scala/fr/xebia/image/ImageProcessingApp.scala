package fr.xebia.image

object ImageProcessingApp extends App {

  private val rawImage = ImageBuilder.fromFile[String]("/input.O.txt")
  val actualImage = rawImage.getOrElse(throw new IllegalArgumentException("Invalid file"))
  val service = ImageProcessingMonad[String](actualImage)
  service.countConnectedElements("#", ".")

  private val segmentedImg = service.threshold(cell => cell == "#", "@")
  segmentedImg.writeToFile("segmented.txt")

}

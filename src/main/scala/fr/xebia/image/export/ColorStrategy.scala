package fr.xebia.image.export

/**
  * Decide the RGB color that should be display at a specified position in image content.
  * Used to convert a text-like image to a true image with 24 bits RGB pixels.
  */
sealed trait ColorStrategy {

  /**
    * Tell which RGB pixel should be at the specified position.
    * @param row row of the pixel in image content
    * @param col col of the pixel in image content
    * @return a 24 bits RGB pixel (red, green, blue).
    */
  def decide(row: Int, col: Int): (Int, Int, Int)

}

/**
  * Create a grey gradient (top - bottom, dark to light).
  */
case object GrayGradientOnHeight extends ColorStrategy {

  override def decide(row: Int, col: Int): (Int, Int, Int) = {
    val pixel = row * 10 + 50
    (pixel, pixel, pixel)
  }

}

case object AlwaysWhite extends ColorStrategy {

  override def decide(row: Int, col: Int): (Int, Int, Int) = {
    val pixel = 255
    (pixel, pixel, pixel)
  }

}


/**
  * Maps image positions to random colors.
  */
case object Rainbow extends ColorStrategy {

  override def decide(row: Int, col: Int): (Int, Int, Int) = {
    val r = scala.util.Random
    (r.nextInt(255), r.nextInt(255), r.nextInt(255))
  }

}


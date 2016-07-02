package fr.xebia.image.export

import java.awt.Color

/**
  * Decide the RGB color that should be display at a specified position in image content.
  * Used to convert a text-like image to a true image with 24 bits RGB pixels.
  */
sealed trait ColorStrategy[-U] {
  /**
    * Tell which RGB pixel should be at the specified position.
    *
    * @param row row of the pixel in image content
    * @param col col of the pixel in image content
    * @return a 24 bits RGB pixel (red, green, blue).
    */
  def decide(row: Int, col: Int, value: U): (Int, Int, Int)

}

/**
  * Create a grey gradient (top - bottom, dark to light).
  */
case object GrayGradientOnHeight extends ColorStrategy[Any] {

  override def decide(row: Int, col: Int, value: Any): (Int, Int, Int) = {
    val pixel = row * 10 + 50
    (pixel, pixel, pixel)
  }

}

case object CharValueGradientOnHeight extends ColorStrategy[String] {

  override def decide(row: Int, col: Int, value: String): (Int, Int, Int) = {
    val brightness = (row * 10 + 50) / 360F
    val saturation = 1F
    val hue = value.sum[Char] / 255F
    val color: Color = Color.getHSBColor(hue,saturation, brightness)
    (color.getRed, color.getGreen, color.getBlue)
  }

}

case object IdentityStrategy extends ColorStrategy[Int] {
  /**
    * Tell which RGB pixel should be at the specified position.
    *
    * @param row row of the pixel in image content
    * @param col col of the pixel in image content
    * @return a 24 bits RGB pixel (red, green, blue).
    */
  override def decide(row: Int, col: Int, value: Int): (Int, Int, Int) = (value, value, value)
}

/**
  * Maps image positions to random colors.
  */
case object Rainbow extends ColorStrategy[Any] {
  type U = AnyRef

  override def decide(row: Int, col: Int, value: Any): (Int, Int, Int) = {
    val r = scala.util.Random
    (r.nextInt(255), r.nextInt(255), r.nextInt(255))
  }

}


package fr.xebia.image

import java.awt.image.{BufferedImage, WritableRaster}
import java.io.File
import javax.imageio.ImageIO

import scala.concurrent.{ExecutionContext, Future}

object ImageWriter {

  import java.io.{BufferedWriter, Closeable, FileOutputStream, OutputStreamWriter}

  private def using[T <: Closeable, R](resource: T)(block: T => R): R = {
    try {
      block(resource)
    }
    finally {
      resource.close()
    }
  }

  /**
    * Write a text image
    * @param fileName file path.
    * @param image image content
    * @tparam U type of pixel value
    */
  def writeToFile[U](fileName: String, image: RawImage[U]): Unit = {
    using(new BufferedWriter(new OutputStreamWriter(new FileOutputStream(fileName)))) { writer =>
      for (x <- image.content) {
        writer.write(x.mkString + "\n")
      }
    }
  }

  /**
    * Write a 24 bit RGB png image, given a filter and a color chooser.
    * @param fileName file path.
    * @param rawImage image content
    * @param keepValue filter function that must return <code>true</code> for pixel value to keep, <code>false</code> for pixel value to be erased (will be replaced by black (0,0,0))
    * @param colorDecider maps a position in the image to a 24 bits RGB pixel (as an INt tuple)
    * @param ec execution context for Futures, implicit
    * @tparam U type of pixel value
    * @return a Future to follow operation completion (Success or Failure) but no value is hold inside the returned Future.
    */
  def writeToImage[U](fileName: String, rawImage: RawImage[U], keepValue: U => Boolean, colorDecider: ColorStrategy)(implicit ec: ExecutionContext): Future[Unit] = {
    Future {
      val image = new BufferedImage(rawImage.width, rawImage.height, BufferedImage.TYPE_INT_RGB)
      val raster = image.getData.asInstanceOf[WritableRaster]
      (0 until rawImage.height).zip(rawImage.content).toList
        .collect { case (rowIndex, rowData) =>
          (0 until rawImage.width).zip(rowData).collect { case (colIndex, pixel) =>
            val (pixelR, pixelG, pixelB) = colorDecider.decide(rowIndex, colIndex)
            //Remember RawImage is adressed as (row, col) so it must be reversed to give (x,y) in screen coordinates
            val x = colIndex
            val y = rowIndex
            if (keepValue(pixel)) {
              raster.setPixel(x, y, Array(pixelR, pixelG, pixelB))
            } else {
              raster.setPixel(x, y, Array(0, 0, 0))
            }
          }
        }
      image.setData(raster)
      ImageIO.write(image, "png", new File(fileName))
    }
  }

}

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

/**
  * Maps image positions to random colors.
  */
case object Rainbow extends ColorStrategy {

  override def decide(row: Int, col: Int): (Int, Int, Int) = {
    val r = scala.util.Random
    (r.nextInt(255), r.nextInt(255), r.nextInt(255))
  }

}

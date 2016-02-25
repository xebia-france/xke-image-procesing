package fr.xebia.image

import java.awt.image.{WritableRaster, BufferedImage}
import java.io.File
import javax.imageio.ImageIO

import scala.concurrent.{Future, ExecutionContext}

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

  def writeToFile[U](fileName: String, image: RawImage[U]): Unit = {
    using(new BufferedWriter(new OutputStreamWriter(new FileOutputStream(fileName)))) { writer =>
      for (x <- image.content) {
        writer.write(x.mkString + "\n")
      }
    }
  }

  def writeToImage[U](fileName: String, rawImage: RawImage[U], contentValue: U => Boolean, colorDecider: ColorStrategy)(implicit ec: ExecutionContext): Future[Unit] = {
    Future {
      val image = new BufferedImage(rawImage.height, rawImage.width, BufferedImage.TYPE_INT_RGB)
      val raster = image.getData.asInstanceOf[WritableRaster]
      rawImage.content.indices.zip(rawImage.content).toList
        .collect { case (rowIndex, rowData) => rowData.indices.zip(rowData).collect { case (colIndex, pixel) =>
          val (pixelR, pixelG, pixelB) = colorDecider.decide(rowIndex, colIndex)
          val actualRow = colIndex
          val actualCol = rowIndex
          if (contentValue(pixel)) {
            raster.setPixel(actualRow, actualCol, Array(pixelR, pixelG, pixelB))
          } else {
            raster.setPixel(actualRow, actualCol, Array(0, 0, 0))
          }
        }
        }
      image.setData(raster)
      ImageIO.write(image, "png", new File(fileName))
    }
  }

}

sealed trait ColorStrategy {
  def decide(row: Int, col: Int): (Int, Int, Int)
}

case object GrayGradientOnHeight extends ColorStrategy {

  override def decide(row: Int, col: Int): (Int, Int, Int) = {
    val pixel = row * 10 + 50
    (pixel, pixel, pixel)
  }

}

case object Rainbow extends ColorStrategy {

  override def decide(row: Int, col: Int): (Int, Int, Int) = {
    val r = scala.util.Random
    (r.nextInt(255), r.nextInt(255), r.nextInt(255))
  }

}

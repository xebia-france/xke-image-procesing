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

  def writeToImage[U](contentValue: U => Boolean, fileName: String, rawImage: RawImage[U])(implicit ec: ExecutionContext): Future[Unit] = {
    Future {
      val image = new BufferedImage(rawImage.height, rawImage.width, BufferedImage.TYPE_INT_RGB)
      val raster = image.getData.asInstanceOf[WritableRaster]
      rawImage.content.indices.zip(rawImage.content).toList
        .collect { case (rowIndex, rowData) => rowData.indices.zip(rowData).collect { case (colIndex, pixel) =>
          val pixelValue = rowIndex * 10 + 50
          val _row = colIndex
          val _col = rowIndex
          if (contentValue(pixel)) {
            raster.setPixel(_row, _col, Array(pixelValue, pixelValue, pixelValue))
          } else {
            raster.setPixel(_row, _col, Array(0, 0, 0))
          }
        }
        }
      image.setData(raster)
      ImageIO.write(image, "png", new File(fileName))
    }
  }

}

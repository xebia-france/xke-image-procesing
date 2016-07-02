package fr.xebia.image

import java.awt.color.ColorSpace
import java.awt.image.{BufferedImage, ColorConvertOp, DataBufferByte}

import fr.xebia.image.core.{ImageProcessingFunctor, RawImage}

import scala.util.Try

object ImageBuilder {

  /**
    * Build an image processing functor from text file path.
    * The content of the file is expected to be "ASCII-art" (each char is a "pixel", each line is the same length).
    *
    * @param fileName a resource path (must be in the classpath)
    * @return Some image processing functor for an image where pixels are characters, or None if any loading error occurs
    */
  def StringImageFromFile(fileName: String): Try[ImageProcessingFunctor[String]] =
    fromFile[String](fileName, (pixel) => pixel.toString)

  /**
    * Build an image processing functor from text file path.
    * The content of the file is expected to be lines of numbers in the range [0..255] separated by spaces.
    *
    * @param fileName a resource path (must be in the classpath)
    * @return Some image processing functor for an image where pixels are integers, or None if any loading error occurs
    */
  def IntImageFromFile(fileName: String): Try[ImageProcessingFunctor[Int]] =
    for {
      input <- Try(FileTools.readImage(fileName))
      contents <- Try(input.map(_.split(" ").toList.map(_.toInt)))
    } yield {
      ImageProcessingFunctor[Int](RawImage[Int](contents))
    }

  def IntImageFromBufferedImage(bufferedImage: BufferedImage): Try[ImageProcessingFunctor[Int]] = Try {
    //1) Ensure image is black&white (for colored image, create a (Byte, Byte, Byte, Byte) image
    val grayImg = new ColorConvertOp(ColorSpace.getInstance(ColorSpace.CS_GRAY), null)
        .filter(bufferedImage, bufferedImage)
    //After that, internal data buffer is still RGBA. Let's convert image to true gray scale
    val width = grayImg.getWidth
    val height = grayImg.getHeight
    val grayScaleImg = new BufferedImage(width, height, BufferedImage.TYPE_BYTE_GRAY)
    val grayScaleImgG2d = grayScaleImg.createGraphics()
    grayScaleImgG2d.drawImage(grayImg, 0, 0, null)
    grayScaleImgG2d.dispose()

    //2) get pixels as list of int. Sign extension is masked to get values from 0 to 255 instead of -128 to 127.
    val data: List[Int] = grayScaleImg.getRaster.getDataBuffer.asInstanceOf[DataBufferByte].getData()
        .map(_.toInt & 0xFF).toList
    //4) make List[List[Int]]
    val contents: List[List[Int]] = data.grouped(width).toList
    ImageProcessingFunctor[Int](RawImage[Int](contents))
  }

  private def fromFile[T](fileName: String, parseChar: Char => T): Try[ImageProcessingFunctor[T]] = {
    for {
      input <- Try(FileTools.readImage(fileName))
      contents <- Try(input.map(_.toCharArray.toList.map(parseChar(_))))
    } yield {
      ImageProcessingFunctor[T](RawImage[T](contents))
    }
  }
}

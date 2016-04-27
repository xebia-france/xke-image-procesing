package fr.xebia.image

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
  def StringImagefromFile(fileName: String): Option[ImageProcessingFunctor[String]] =
    fromFile[String](fileName, (pixel) => pixel.toString)

  private def fromFile[T](fileName: String, parseChar: Char => T): Option[ImageProcessingFunctor[T]] = {
    (for {
      input <- Try(FileTools.readImage(fileName))
      contents <- Try(input.map(_.toCharArray.toList.map(parseChar(_))))
    } yield {
      ImageProcessingFunctor[T](RawImage[T](contents))
    }).toOption
  }

  /**
    * Build an image processing functor from text file path.
    * The content of the file is expected to be lines of numbers in the range [0..255] separated by spaces.
    *
    * @param fileName a resource path (must be in the classpath)
    * @return Some image processing functor for an image where pixels are integers, or None if any loading error occurs
    */
  def IntImagefromFile(fileName: String): Option[ImageProcessingFunctor[Int]] =
    (for {
      input <- Try(FileTools.readImage(fileName))
      contents <- Try(input.map(_.split(" ").toList.map(_.toInt)))
    } yield {
      ImageProcessingFunctor[Int](RawImage[Int](contents))
    }).toOption

}

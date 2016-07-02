package fr.xebia.image

import java.awt.image.BufferedImage
import java.io.FileNotFoundException
import javax.imageio.ImageIO

import scala.util.Try

object FileTools {

  /**
    * Fully reads a resource with specified path.
    *
    * @param fileName a resource path (must be in the classpath ; it is relative to this class package if doesn't start by a '/')
    * @return the content of the file as a list of lines.
    * @throws java.io.IOException on any I/O error
    */
  def readImage(fileName: String): List[String] = scala.io.Source
    .fromInputStream(getClass.getResourceAsStream(fileName)).getLines().toList

  def readPixelImage(fileName: String): Try[BufferedImage] = {
    Try{
      val resourceStream = getClass.getResourceAsStream(fileName)
      if(resourceStream == null) throw new FileNotFoundException(s"$fileName (getClass.getResourceAsStream returned null)")
      else ImageIO.read(resourceStream)
    }
  }



}

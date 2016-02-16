package fr.xebia.image

object FileTools {

  def readImage(fileName: String): List[String] = scala.io.Source
    .fromInputStream(getClass.getResourceAsStream(fileName)).getLines().toList

}

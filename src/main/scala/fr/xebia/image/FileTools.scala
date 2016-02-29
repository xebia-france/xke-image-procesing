package fr.xebia.image

object FileTools {
	/**
      * Fully reads a resource with specified path.
      * @param fileName a resource path (must be in the classpath ; it is relative to this class package if doesn't start by a '/')
      * @return the content of the file as a list of lines.
      * @throws java.io.IOException on any I/O error
      */
  def readImage(fileName: String): List[String] = scala.io.Source
    .fromInputStream(getClass.getResourceAsStream(fileName)).getLines().toList

}

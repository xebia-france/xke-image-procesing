package fr.xebia.image

import scala.annotation.tailrec
import scala.util.Try

object ImageBuilder {

  def fromFile[T](fileName: String): Option[RawImage[T]] = {
    (for {
      input <- Try(FileTools.readImage(fileName))
      contents <- Try(input.map(_.toCharArray.toList.map(_.asInstanceOf[T])))
    } yield {
      RawImage[T](contents)
    }).toOption
  }

}

case class Position(x: Int, y: Int) {
  override def toString: String = s"($x,$y)"
}

case class RawImage[U](content: List[List[U]]) {

  val width = content.size

  // TODO: verify if that all rows have the same size
  val height = content.head.size

  def getFirstThatMatches(searched: U): Option[Position] = {
    val zipped: List[(Int, List[U])] = content.indices
      .toList
      .zip(content)
    zipped
      .find { case (index, row) => row.contains(searched) }
      .map { case (index, row) => Position(index, row.indexOf(searched)) }
  }

  def takeWhile(predicate: U => Boolean): List[U] =
    content.collect { case (row) => row.filter(predicate(_)) }.flatten

  def replace(neighborList: List[Position], value: U): RawImage[U] = {
    @tailrec
    def go(updatedImage: RawImage[U], remainingNeighbor: List[Position]): RawImage[U] = {
      remainingNeighbor match {
        case Nil =>
          updatedImage

        case currentPos :: remainingPositions =>
          val newContent = updatedImage.content.updated(
            currentPos.x,
            updatedImage.content(currentPos.x).updated(currentPos.y, value))
          go(copy(content = newContent), remainingPositions)
      }
    }
    go(this, neighborList)
  }

  def neighborsOnly(center: Position): List[Position] = {
    val neigh = scala.collection.mutable.ArrayBuffer.empty[Position]
    neigh += center.copy(x = center.x - 1, y = center.y - 1)
    neigh += center.copy(x = center.x - 1)
    neigh += center.copy(x = center.x - 1, y = center.y + 1)
    neigh += center.copy(y = center.y + 1)
    neigh += center.copy(y = center.y + 1, x = center.x + 1)
    neigh += center.copy(y = center.y - 1)
    neigh += center.copy(y = center.y - 1, x = center.x + 1)
    neigh += center.copy(x = center.x + 1)

    neigh
      .toList
      .filter(pos => pos.x >= 0 && pos.y >= 0)
      .filter(pos => pos.x < content.size && pos.y < content.head.size)
  }

  def neighborsAndSelf(center: Position): List[Position] =
    neighborsOnly(center) :+ center

  def at(pos: Position): U = content(pos.x)(pos.y)

  import java.io.{BufferedWriter, Closeable, FileOutputStream, OutputStreamWriter}

  private def using[T <: Closeable, R](resource: T)(block: T => R): R = {
    try {
      block(resource)
    }
    finally {
      resource.close()
    }
  }

  def writeToFile(fileName: String): Unit = {
    using(new BufferedWriter(new OutputStreamWriter(new FileOutputStream(fileName)))) { writer =>
      for (x <- content) {
        writer.write(x.mkString + "\n")
      }
    }
  }

}

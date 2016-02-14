package fr.xebia.image

import scala.util.Try

case class Position(x: Int, y: Int) {
  override def toString: String = s"($x,$y)"
}

object ImageProcessing extends App {

  lazy val input: List[String] = scala.io.Source
    .fromInputStream(getClass.getResourceAsStream("/input.O.txt")).getLines().toList

  lazy val dimension = Try(
    input.take(1).head.split(" ")
      .toList
      .map(_.toInt) match {
      case h :: t :: Nil => Position(h - 1, t - 1)
      case _ => throw new IllegalArgumentException("Invalid Config")
    }
  ).get

  lazy val contents = input.drop(1).map(_.toCharArray.toList.map(_.toString))

  private val maze = RawImage[String](contents, dimension)
  MazeService.resolve(maze, "#", "@")

}

object MazeService {


  def resolve[U](maze: RawImage[U], contentValue: U, replaceValue: U): Int = {
    //val corner = maze.at(Position(9, 16))
    //println(s"corner <$corner>")
    println("")
    val seedPosition = Position(0, 6)
    val neighbors = maze.neighborsAndSelf(seedPosition)

    /*val newMaze = maze.replace(neighbors, replaceValue)
    newMaze.writeToFile("result.txt")*/

    val connectedPoints = propagateFront[U](maze, neighbors, contentValue, replaceValue)
    val newMaze = maze.replace(connectedPoints, replaceValue)
    newMaze.writeToFile("result.txt")
    //println(s"first value <$newMaze>")
    0
  }


  def propagateFront[U](baseImage: RawImage[U], neighbors: List[Position], searchedValue: U, frontMark: U): List[Position] = {

    def go(maze: RawImage[U], neighbors: List[Position], positions: List[Position], pos: Int): List[Position] = {
      neighbors.filter(maze.at(_) == searchedValue) match {
        case Nil =>
          positions
        case currentSeed :: remainingSeed =>
          val newNeighborhood: List[Position] = maze.neighborsAndSelf(currentSeed)
          val newImage = maze.replace(List(currentSeed), frontMark)
          newImage.writeToFile(s"result-$pos.txt")
          go(newImage, remainingSeed ++ newNeighborhood, positions :+ currentSeed, pos + 1)
      }
    }

    go(baseImage.copy(), neighbors, List.empty[Position], 0)
  }

}


case class RawImage[U](content: List[List[U]], limits: Position) {

  def replace(neighborList: List[Position], value: U): RawImage[U] = {
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
      .filter(pos => pos.x <= limits.x && pos.y <= limits.y)
  }

  def neighborsAndSelf(center: Position): List[Position] = {
    val neigh = scala.collection.mutable.ArrayBuffer.empty[Position]
    neigh += center.copy(x = center.x - 1, y = center.y - 1)
    neigh += center.copy(x = center.x - 1)
    neigh += center.copy(x = center.x - 1, y = center.y + 1)
    neigh += center.copy(y = center.y + 1)
    neigh += center
    neigh += center.copy(y = center.y + 1, x = center.x + 1)
    neigh += center.copy(y = center.y - 1)
    neigh += center.copy(y = center.y - 1, x = center.x + 1)
    neigh += center.copy(x = center.x + 1)

    neigh
      .toList
      .filter(pos => pos.x >= 0 && pos.y >= 0)
      .filter(pos => pos.x <= limits.x && pos.y <= limits.y)
  }

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


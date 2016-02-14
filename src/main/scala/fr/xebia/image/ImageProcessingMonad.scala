package fr.xebia.image

case class ImageProcessingMonad[U](rawImage: RawImage[U]) {

  //val corner = maze.at(Position(9, 16))
  def countUnconnectedElements(contentValue: U, replaceValue: U): Int = {
    println("")
    val seedPosition = Position(0, 6)
    val neighbors = rawImage.neighborsAndSelf(seedPosition)
    val connectedPoints = propagateFront(neighbors, contentValue, replaceValue)
    val newImage = rawImage.replace(connectedPoints, replaceValue)
    //newImage.writeToFile("result.txt")
    //println(s"first value <$newMaze>")
    0
  }

  private[image] def propagateFront(neighbors: List[Position], searchedValue: U, frontMark: U): List[Position] = {
    def go(maze: RawImage[U], neighbors: List[Position], positions: List[Position]): List[Position] = {
      neighbors.filter(maze.at(_) == searchedValue) match {
        case Nil =>
          positions
        case currentSeed :: remainingSeed =>
          val newNeighborhood: List[Position] = maze.neighborsAndSelf(currentSeed)
          val newImage = maze.replace(List(currentSeed), frontMark)
          go(newImage, remainingSeed ++ newNeighborhood, positions :+ currentSeed)
      }
    }
    go(rawImage.copy(), neighbors, List.empty[Position])
  }

  def threshold(f: U => Boolean, replaceBy: U): ImageProcessingMonad[U] =
    map(cell => if (f(cell)) replaceBy else cell)

  def map[R](f: U => R): ImageProcessingMonad[R] = {
    ImageProcessingMonad[R](
      RawImage(
        rawImage.content.map(_.map(cell => f(cell)))
      ))
  }
}


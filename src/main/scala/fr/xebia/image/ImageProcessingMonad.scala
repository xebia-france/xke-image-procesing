package fr.xebia.image

case class ImageProcessingMonad[U](rawImage: RawImage[U]) {

  //val corner = maze.at(Position(9, 16))
  // TODO: implement
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

  private[image] def propagateFront(seeds: List[Position], searchedValue: U, markWith: U): List[Position] = {
    def go(imageCopy: RawImage[U], neighbors: List[Position], positions: List[Position]): List[Position] = {
      neighbors.filter(imageCopy.at(_) == searchedValue) match {
        case Nil =>
          positions
        case currentSeed :: remainingSeed =>
          val newNeighborhood: List[Position] = imageCopy.neighborsAndSelf(currentSeed)
          val newImage = imageCopy.replace(List(currentSeed), markWith)
          go(newImage, remainingSeed ++ newNeighborhood, positions :+ currentSeed)
      }
    }
    go(rawImage, seeds, List.empty[Position])
  }

  def getFirstThatMatches(searched: U): Option[Position] = {
    val zipped: List[(Int, List[U])] = rawImage.content.indices
      .toList
      .zip(rawImage.content)
    zipped
      .find { case (index, row) => row.contains(searched) }
      .map { case (index, row) => Position(index, row.indexOf(searched))}
  }

  def replace(neighborList: List[Position], value: U): ImageProcessingMonad[U] =
    ImageProcessingMonad(rawImage.replace(neighborList, value))

  def threshold(f: U => Boolean, replaceBy: U): ImageProcessingMonad[U] =
    map(cell => if (f(cell)) replaceBy else cell)

  def map[R](f: U => R): ImageProcessingMonad[R] = {
    ImageProcessingMonad[R](
      RawImage(
        rawImage.content.map(_.map(cell => f(cell)))
      ))
  }
}


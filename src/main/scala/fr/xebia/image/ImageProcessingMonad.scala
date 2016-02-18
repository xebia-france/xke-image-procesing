package fr.xebia.image

import java.awt.image.{BufferedImage, WritableRaster}
import java.io.File
import javax.imageio.ImageIO

import scala.annotation.tailrec
import scala.concurrent.{ExecutionContext, Future}

trait BaseImageTools[U] {

  val rawImage: RawImage[U]

  def neighborsAndSelf(center: Position): List[Position] = rawImage.neighborsAndSelf(center)

  def getFirstThatMatches(searched: U): Option[Position] =
    getFirstThatMatchesOn(rawImage, searched)

  protected def getFirstThatMatchesOn(onImage: RawImage[U], searched: U): Option[Position] =
    onImage.getFirstThatMatches(searched)

  def replace(neighborList: List[Position], value: U): ImageProcessingMonad[U] =
    new ImageProcessingMonad(rawImage.replace(neighborList, value))

  def at(pos: Position): U = rawImage.at(pos)

  def writeToFile(fileName: String): Unit = rawImage.writeToFile(fileName)

  def writeToImage(contentValue: U, fileName: String)(implicit ec: ExecutionContext): Future[Unit] = {
    Future {
      val image = new BufferedImage(rawImage.height, rawImage.width, BufferedImage.TYPE_INT_RGB)
      val raster = image.getData.asInstanceOf[WritableRaster]
      rawImage.content.indices.zip(rawImage.content).toList
        .collect { case (rowIndex, rowData) => rowData.indices.zip(rowData).collect { case (colIndex, pixel) =>
          val pixelValue = rowIndex * 10 + 50
          val _row = colIndex
          val _col = rowIndex
          if (pixel == contentValue) {
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

case class ImageProcessingMonad[U](rawImage: RawImage[U]) extends BaseImageTools[U] {

  def countConnectedElements(contentValue: U, emptyValue: U): Int = {
    @tailrec
    def go(copyImage: RawImage[U], maybePosition: Option[Position], connectedElements: Int): Int = {
      maybePosition match {
        case None =>
          connectedElements

        case Some(seed) =>
          val connectedPoints = propagateFront(
            copyImage.neighborsAndSelf(seed),
            contentValue,
            emptyValue
          )
          val newImage = copyImage.replace(connectedPoints, emptyValue)
          go(newImage, getFirstThatMatchesOn(newImage, contentValue), connectedElements + 1)
      }
    }
    go(this.rawImage, getFirstThatMatches(contentValue), 0)
  }

  private[image] def propagateFront(seeds: List[Position], searchedValue: U, markWith: U): List[Position] = {
    @tailrec
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

  def threshold(f: U => Boolean, replaceBy: U): ImageProcessingMonad[U] =
    map[U](cell => if (f(cell)) replaceBy else cell)

  def map[R](f: U => R): ImageProcessingMonad[R] = {
    new ImageProcessingMonad[R](
      RawImage(
        rawImage.content.map(_.map(cell => f(cell)))
      )
    )
  }

}

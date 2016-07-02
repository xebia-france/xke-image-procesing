package fr.xebia.image

import fr.xebia.image.core.RawImage
import fr.xebia.image.export.{IdentityStrategy, ImageWriter}
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.{FunSpec, Inside, Matchers}

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext}
import scala.util.{Success, Try}

class ImageBuilderSpec extends FunSpec with Matchers with ScalaFutures with Inside{
  describe("An image builder") {
    it("should convert an image to an ImageProcessingFuncor[Int] of same dimensions") {
      val tryRawImage: Try[RawImage[Int]] = (for {
        buffImg <- FileTools.readPixelImage("/cells.png")
        functor <- ImageBuilder.IntImageFromBufferedImage(buffImg)
      } yield functor).map(_.rawImage)

      inside(tryRawImage) {
        case Success(rawImage) =>
          rawImage.width should be(219)
          rawImage.height should be(218)
      }
    }
    it("should put the rigth gray value into content of RawImage[Int]") {
      val tryRawImage: Try[RawImage[Int]] = (for {
        buffImg <- FileTools.readPixelImage("/cells.png")
        functor <- ImageBuilder.IntImageFromBufferedImage(buffImg)
      } yield functor).map(_.rawImage)

      inside(tryRawImage) {
        case Success(rawImage) =>
          //check some samples. Expected values checked using The GIMP!
          rawImage.content.head.slice(0, 10) should be(List(37, 73, 73, 72, 72, 74, 72, 71, 73, 73))
          rawImage.content(50).slice(0, 20) should be(List(95, 196, 204, 214, 232, 252, 255, 220, 113, 17, 0, 0, 0, 2, 43, 155, 245, 255, 255, 246))

          /*
          Uncomment the following to generate a PNG from the RawImage.
          You'll see it is identical to cells.png except for the border pixels (original is transparent, so it is opacified by our gray scale filter)
          */
          //implicit val ec: ExecutionContext = scala.concurrent.ExecutionContext.global
          //Await.ready(ImageWriter.writeToImage("ImageBuilderSpec.png", rawImage, (v: Int) => true, IdentityStrategy), 30.seconds)
      }

    }
  }
}

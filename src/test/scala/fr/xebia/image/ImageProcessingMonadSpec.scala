package fr.xebia.image

import fr.xebia.image.TestFactory.ImagingTools._
import fr.xebia.image.TestFactory._
import org.scalatest.concurrent.{PatienceConfiguration, ScalaFutures}
import org.scalatest.time.{Seconds, Span}
import org.scalatest.{FunSpec, Matchers}

class ImageProcessingMonadSpec extends FunSpec with Matchers with ScalaFutures {

  describe("a segmentation monad") {

    it("should replace '#' by '@'") {
      // given an image
      val monad = anImageWrapperFrom(
        """
          |......###........
          |...###...##......
          |..##.......##....
          |..#..........#...
          |..#..........#...
          |..#.........##...
          |..###......###...
          |...###...###.....
          |.....####........
          |.................
        """.stripMargin)

      // when
      val segmentedImg = monad.threshold(
        cell => cell == "#",
        replaceBy = "@"
      )

      // then
      segmentedImg shouldBe anImageWrapperFrom(
        """
          |......@@@........
          |...@@@...@@......
          |..@@.......@@....
          |..@..........@...
          |..@..........@...
          |..@.........@@...
          |..@@@......@@@...
          |...@@@...@@@.....
          |.....@@@@........
          |.................
        """.stripMargin
      )
    }

    it("should detect a missing first match in the image") {
      val monad = anImageWrapperFrom(
        """|......###........
          |...###...##......
          |..##.......##....
          |..############...
          |.................
          |.................
          |..############...
          |...#########.....
          |.....####........
          |.................
        """.stripMargin)
      monad.getFirstThatMatches("&") shouldNot be(defined)
    }

    it("should detect unconnected elements in an image from disk") {
      val monad = ImageBuilder.StringImagefromFile("/google.txt").get
      monad.countConnectedElements(
        contentValue = "#",
        emptyValue = "."
      ) shouldBe 6
    }

    it("should detect the right amt of characters from a file containing 'xebia' ") {
      val monad = ImageBuilder.StringImagefromFile("/xebia.txt").get
      monad.countConnectedElements(
        contentValue = "#",
        emptyValue = "."
      ) shouldBe 5
    }

  }

  describe("a segmentation monad executing front propagation") {

    val anImageWrapper = anImageWrapperFrom(
      """|......###........
        |...###...##......
        |..##.......##....
        |..############...
        |.................
        |.................
        |..############...
        |...#########.....
        |.....####........
        |.................
      """.stripMargin)

    it("should propagate a front from a specified seed") {
      // given
      val specialChar = "@"
      val seed = Position(0, 7)

      // when
      val anImageWrapper = anImageWrapperFrom(
        """
          |......###........
          |...###...##......
          |..##.......##....
          |..############...
          |..#..........#...
          |..#.........##...
          |..############...
          |...###...###.....
          |.....####........
          |.................
        """.stripMargin)

      val segmentedPositions = anImageWrapper.propagateFront(
        seeds = anImageWrapper.neighborsAndSelf(seed),
        searchedValue = "#",
        markWith = specialChar
      )
      val segmentedImage = anImageWrapper.replace(segmentedPositions, specialChar)

      // then
      segmentedImage shouldBe anImageWrapperFrom(
        """
          |......@@@........
          |...@@@...@@......
          |..@@.......@@....
          |..@@@@@@@@@@@@...
          |..@..........@...
          |..@.........@@...
          |..@@@@@@@@@@@@...
          |...@@@...@@@.....
          |.....@@@@........
          |.................
        """.stripMargin
      )
    }

    it("should propagate a front from the first value that matches") {
      // given
      val firstFrontMonad = anImageWrapper
      val firstSeed = aSeedThatMatches(firstFrontMonad, Position(0, 6), "#")

      // when the first monad is called
      val firstFront: List[Position] = firstFrontMonad.propagateFront(
        seeds = List(firstSeed),
        searchedValue = "#",
        markWith = "@"
      )
      firstFront shouldNot be(empty)

      // when the second monad
      val secondFrontMonad = firstFrontMonad.replace(firstFront, "@")
      val secondSeed = aSeedThatMatches(secondFrontMonad, Position(6, 2), "#")

      val secondFront: List[Position] = secondFrontMonad.propagateFront(
        seeds = List(secondSeed),
        searchedValue = "#",
        markWith = "&"
      )
      secondFrontMonad.replace(secondFront, "&") shouldBe anImageWrapperFrom(
        """
          |......@@@........
          |...@@@...@@......
          |..@@.......@@....
          |..@@@@@@@@@@@@...
          |.................
          |.................
          |..&&&&&&&&&&&&...
          |...&&&&&&&&&.....
          |.....&&&&........
          |.................
        """.stripMargin
      )
    }

    it("should detect unconnected elements in an image") {
      val monad = anImageWrapperFrom(
        """
          |.................
          |...##......##....
          |...##......##....
          |...##......##....
          |.................
          |.................
          |..##........##...
          |...##......##....
          |.....######......
          |.................
        """.stripMargin)
      monad.countConnectedElements(
        contentValue = "#",
        emptyValue = "."
      ) shouldBe 3
    }

  }

  describe("a exporter mechanism") {
    import scala.concurrent.ExecutionContext.Implicits.global
    val timeout = PatienceConfiguration.Timeout(Span(2, Seconds))

    it("should read and write a String image") {
      import java.nio.file.{Files, Paths}
      val fileName: String = "output.png"
      val monad = anImageWrapperFrom(
        """
          |.................
          |...##......##....
          |...##......##....
          |...##......##....
          |.................
          |.................
          |..##........##...
          |...##......##....
          |.....######......
          |.................
        """.stripMargin)

      val eventualUnit = ImageWriter.writeToImage(
        fileName,
        monad.rawImage,
        (pixel: String) => pixel == "#",
        GrayGradientOnHeight)
      whenReady(eventualUnit, timeout) { response =>
        Files.exists(Paths.get(fileName))
      }
    }

    it("should read and write an Int image") {
      import java.nio.file.{Files, Paths}
      val fileName: String = "outputColor.png"
      val numberMonad = ImageBuilder.IntImagefromFile("/input.txt").get
      val eventualUnit = ImageWriter.writeToImage(
        fileName,
        numberMonad.rawImage,
        (pixel: Int) => pixel == 255,
        Rainbow)
      whenReady(eventualUnit, timeout) { response =>
        Files.exists(Paths.get(fileName))
      }
    }

  }

}

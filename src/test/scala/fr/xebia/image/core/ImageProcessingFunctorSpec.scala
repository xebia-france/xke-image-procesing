package fr.xebia.image.core

import fr.xebia.image.ImageBuilder
import fr.xebia.image.TestFactory.ImagingTools._
import fr.xebia.image.TestFactory._
import fr.xebia.image.export.{GrayGradientOnHeight, ImageWriter, Rainbow}
import org.scalatest.concurrent.{PatienceConfiguration, ScalaFutures}
import org.scalatest.time.{Seconds, Span}
import org.scalatest.{FunSpec, Matchers}

class ImageProcessingFunctorSpec extends FunSpec with Matchers with ScalaFutures {

  describe("A segmentation functor") {

    describe("handling hardcoded images") {

      it("should display the right content") {
        val image = anImageFunctorFrom(
          """
            |..##..
            |##..##
            |..##..
          """.stripMargin)

        val imageAsString = image.toString

        imageAsString shouldBe
          """..##..
            |##..##
            |..##..""".stripMargin
      }

      it("should replace '#' by '@'") {
        // given an image
        val functor = anImageFunctorFrom(
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
        val segmentedImg = functor.threshold(
          cell => cell == "#",
          replaceBy = "@"
        )

        // then
        segmentedImg shouldBe anImageFunctorFrom(
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
        val aFunctor = anImageFunctorFrom(
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
        aFunctor.firstThatMatches("&") shouldNot be(defined)
      }

      it("should apply threshold on a matching predicate") {
        val anImageFunctor = anImageFunctorFrom(
          """
            |-#--#----
            |-#--#--#-
            |-#--#--#-
            |-#--#----
          """.stripMargin)
        val expectedContent = anImageFunctorFrom(
          """
            |---------
            |---------
            |---------
            |---------
          """.stripMargin
        )
        anImageFunctor.threshold(
          predicate = (p) => p == "#",
          replaceBy = "-") shouldBe expectedContent
      }

      it("should not apply threshold if none match the predicate") {
        val anImageFunctor = anImageFunctorFrom("-#--#----")
        val expectedContent = anImageFunctorFrom("-#--#----")
        anImageFunctor.threshold(
          predicate = (p) => p == "@",
          replaceBy = "-") shouldBe expectedContent
      }

      it("should propagate a front from a simple image") {
        val aContent =
          """
            |-#--#----
            |-#--#--#-
            |-#--#--#-
            |-#--#----
          """.stripMargin
        val anImageFunctor = anImageFunctorFrom(aContent)
        anImageFunctor.countConnectedElements("#", "-") shouldBe 3
      }

    }

    describe("reading files") {

      it("should detect unconnected elements in an image from disk") {
        val aFunctor = ImageBuilder.StringImageFromFile("/google.txt").get
        aFunctor.countConnectedElements(
          contentValue = "#",
          emptyValue = "."
        ) shouldBe 6
      }

      it("should detect the right amt of characters from a file containing 'xebia' ") {
        val aFunctor = ImageBuilder.StringImageFromFile("/xebia.txt").get
        aFunctor.countConnectedElements(
          contentValue = "#",
          emptyValue = "."
        ) shouldBe 5
      }

    }

    describe("executing front propagation") {

      val anImageFunctor = anImageFunctorFrom(
        """
          |......###........
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
        val seed = Position(7, 0)

        // when
        val anImageFunctor = anImageFunctorFrom(
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

        val segmentedPositions = anImageFunctor.propagateFront(
          seeds = anImageFunctor.neighborsAndSelf(seed),
          searchedValue = "#",
          markWith = specialChar
        )
        val segmentedImage = anImageFunctor.replace(segmentedPositions, specialChar)

        // then
        segmentedImage shouldBe anImageFunctorFrom(
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
        val firstFrontFunctor = anImageFunctor
        val firstSeed = aSeedThatMatches(firstFrontFunctor, Position(6, 0), "#")

        // when the first functor is called
        val firstFront: List[Position] = firstFrontFunctor.propagateFront(
          seeds = List(firstSeed),
          searchedValue = "#",
          markWith = "@"
        )
        firstFront shouldNot be(empty)

        // when the second functor
        val secondFrontFunctor = firstFrontFunctor.replace(firstFront, "@")
        val secondSeed = aSeedThatMatches(secondFrontFunctor, Position(2, 6), "#")

        val secondFront: List[Position] = secondFrontFunctor.propagateFront(
          seeds = List(secondSeed),
          searchedValue = "#",
          markWith = "&"
        )
        secondFrontFunctor.replace(secondFront, "&") shouldBe anImageFunctorFrom(
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
        val anImageFunctor = anImageFunctorFrom(
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
        anImageFunctor.countConnectedElements(
          contentValue = "#",
          emptyValue = "."
        ) shouldBe 3
      }

    }

  }

  describe("An exporter mechanism") {
    import scala.concurrent.ExecutionContext.Implicits.global
    val timeout = PatienceConfiguration.Timeout(Span(2, Seconds))

    it("should read and write a String image") {
      import java.nio.file.{Files, Paths}
      val aFileName: String = "output.png"
      val anImageFunctor = anImageFunctorFrom(
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

      val eventualResponse = ImageWriter.writeToImage(
        aFileName,
        anImageFunctor.rawImage,
        (pixel: String) => pixel == "#",
        GrayGradientOnHeight)
      whenReady(eventualResponse, timeout) { response =>
        Files.exists(Paths.get(aFileName))
      }
    }

    it("should read and write an Int image") {
      import java.nio.file.{Files, Paths}
      val aFileName = "outputColor.png"
      val aNumericImageFunctor = ImageBuilder.IntImageFromFile("/input.txt").get
      val eventualResponse = ImageWriter.writeToImage(
        aFileName,
        aNumericImageFunctor.rawImage,
        (pixel: Int) => pixel == 255,
        Rainbow)
      whenReady(eventualResponse, timeout) { response =>
        Files.exists(Paths.get(aFileName))
      }
    }

  }

}

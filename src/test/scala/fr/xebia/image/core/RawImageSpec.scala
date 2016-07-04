package fr.xebia.image.core

import fr.xebia.image.TestFactory._
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.{FunSpec, Matchers}

class RawImageSpec extends FunSpec with Matchers with ScalaFutures {

  describe("an image wrapper") {

    val aDummyContent = List(
      List("a", "b"),
      List("c", "d"),
      List("c", "e")
    )

    describe("validating input") {

      it("should detect an empty input") {
        intercept[IllegalArgumentException] {
          aStringImage.filledWith(List.empty)
        }
      }

      it("should detect images having rows with different sizes") {
        val anErroneousContent = List(
          List("a", "b"),
          List("a")
        )
        intercept[IllegalArgumentException] {
          aStringImage.filledWith(anErroneousContent)
        }
      }

    }

    describe("getting data on content") {
      val aRealisticContent =
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
        """.stripMargin

      it("should return the right content at the position specified") {
        val anImage = aStringImage.filledWith(aDummyContent)

        anImage at Position(0, 0) shouldBe "a"
        anImage at Position(0, 1) shouldBe "c"
        anImage at Position(0, 2) shouldBe "c"

        anImage at Position(1, 0) shouldBe "b"
        anImage at Position(1, 1) shouldBe "d"
        anImage at Position(1, 2) shouldBe "e"
      }

      it("should display the right content") {
        val image = aStringImage.filledWith(aDummyContent)

        val imageAsString = image.toString

        imageAsString shouldBe
          """ab
            |cd
            |ce""".stripMargin
      }

      it("TODO 03 - should filter the neighborhood of a pixel close to the border") {
        val anImage = anImageFunctorFrom(aRealisticContent).rawImage
        anImage.neighborsOnly(Position(0, 0)).sorted shouldBe Seq(
          Position(1, 0),
          Position(1, 1),
          Position(0, 1)
        ).sorted
        anImage.neighborsAndSelf(Position(0, 0)).sorted shouldBe Seq(
          Position(0, 0),
          Position(1, 0),
          Position(1, 1),
          Position(0, 1)
        ).sorted
      }

      it("TODO 03 - should get the neighborhood of a pixel not in the border") {
        val x = 8
        val y = 5
        val anImage = anImageFunctorFrom(aRealisticContent).rawImage
        anImage.neighborsOnly(Position(x, y)).sorted shouldBe Seq(
          // first line
          Position(x - 1, y - 1),
          Position(x, y - 1),
          Position(x + 1, y - 1),
          // second line
          Position(x - 1, y),
          Position(x + 1, y),
          // third line
          Position(x - 1, y + 1),
          Position(x, y + 1),
          Position(x + 1, y + 1)
        ).sorted
        anImage.neighborsAndSelf(Position(x, y)).sorted shouldBe Seq(
          // first line
          Position(x - 1, y - 1),
          Position(x, y - 1),
          Position(x + 1, y - 1),
          // second line
          Position(x - 1, y),
          Position(x, y),
          Position(x + 1, y),
          // third line
          Position(x - 1, y + 1),
          Position(x, y + 1),
          Position(x + 1, y + 1)
        ).sorted
      }

      it("TODO 01 - should get the first that matches ") {
        val anImage = aStringImage.filledWith(aDummyContent)
        anImage firstThatMatches "a" shouldBe Some(Position(0, 0))
        anImage firstThatMatches "c" shouldBe Some(Position(0, 1))
        anImage firstThatMatches "e" shouldBe Some(Position(1, 2))
      }

      it("should return None when no match found ") {
        val anImage = aStringImage.filledWith(aDummyContent)
        anImage firstThatMatches "x" shouldBe None
        anImage firstThatMatches "y" shouldBe None
      }

    }

    describe("processing data") {

      it("TODO 02 - should replace the position specified") {
        val positionsToReplace = List(Position(0, 0), Position(1, 0))
        val expectedImage = RawImage[String](List(
          List("@", "@"),
          List("c", "d"),
          List("c", "e")
        ))

        aStringImage.filledWith(aDummyContent).replace(positionsToReplace, "@") shouldBe expectedImage
      }

    }

  }

}



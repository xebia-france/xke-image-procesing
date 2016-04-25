package fr.xebia.image

import fr.xebia.image.TestFactory.aStringImage
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.{FunSpec, Matchers}

class RawImageSpec extends FunSpec with Matchers with ScalaFutures {

  describe("an image wrapper validating input") {

    it("should detect an empty input") {
      intercept[IllegalArgumentException] {
        aStringImage.filledWith(List.empty)
      }
    }

    it("should detect images having rows with different sizes") {
      val input = List(
        List("a", "b"),
        List("a")
      )
      intercept[IllegalArgumentException] {
        aStringImage.filledWith(input)
      }
    }

  }

  describe("an image wrapper processing data") {

    val aContent = List(
      List("a", "b"),
      List("c", "d"),
      List("c", "e")
    )

    it("should get the first that matches ") {
      val anImage = aStringImage.filledWith(aContent)
      anImage firstThatMatches "a" shouldBe Some(Position(0, 0))
      anImage firstThatMatches "c" shouldBe Some(Position(0, 1))
      anImage firstThatMatches "e" shouldBe Some(Position(1, 2))
    }

    it("should return None when no match found ") {
      val anImage = aStringImage.filledWith(aContent)
      anImage firstThatMatches "x" shouldBe None
      anImage firstThatMatches "y" shouldBe None
    }

    it("should replace the position specified") {
      val positionsToReplace = List(Position(0, 0), Position(1, 0))
      val expectedImage = RawImage[String](List(
        List("@", "@"),
        List("c", "d"),
        List("c", "e")
      ))

      aStringImage.filledWith(aContent).replace(positionsToReplace, "@") shouldBe expectedImage
    }

    it("should return the right content at the position specified") {
      val anImage = aStringImage.filledWith(aContent)

      anImage at Position(0, 0) shouldBe "a"
      anImage at Position(0, 1) shouldBe "c"
      anImage at Position(0, 2) shouldBe "c"

      anImage at Position(1, 0) shouldBe "b"
      anImage at Position(1, 1) shouldBe "d"
      anImage at Position(1, 2) shouldBe "e"
    }

  }

}



package fr.xebia.image

import org.scalatest.concurrent.ScalaFutures
import org.scalatest.{FunSpec, Matchers}

class RawImageSpec extends FunSpec with Matchers with ScalaFutures {

  describe("an image wrapper validating input") {

    it("should detect an empty input") {
      intercept[IllegalArgumentException] {
        RawImage[String](List.empty)
      }
    }

    it("should detect images having rows with different sizes") {
      val input = List(
        List("a", "b"),
        List("a")
      )
      intercept[IllegalArgumentException] {
        RawImage[String](input)
      }
    }

  }

  describe("an image wrapper processing data") {

    val input = List(
      List("a", "b"),
      List("c", "d"),
      List("c", "e")
    )

    it("should get the first that matches ") {
      RawImage[String](input).getFirstThatMatches("a") shouldBe Some(Position(0, 0))
      RawImage[String](input).getFirstThatMatches("c") shouldBe Some(Position(1, 0))
      RawImage[String](input).getFirstThatMatches("e") shouldBe Some(Position(2, 1))
    }

    it("should return None when no match found ") {
      RawImage[String](input).getFirstThatMatches("x") shouldBe None
      RawImage[String](input).getFirstThatMatches("y") shouldBe None
    }

    it("should replace the position specified") {
      val positions: List[Position] = List(Position(0, 0), Position(0, 1))
      val expected = RawImage[String](List(
        List("@", "@"),
        List("c", "d"),
        List("c", "e")
      ))

      RawImage[String](input).replace(positions, "@") shouldBe expected
    }

    it("should return the value at the value specified") {
      RawImage[String](input).at(Position(0, 0)) shouldBe "a"
      RawImage[String](input).at(Position(0, 1)) shouldBe "b"
      RawImage[String](input).at(Position(1, 0)) shouldBe "c"
      RawImage[String](input).at(Position(1, 1)) shouldBe "d"
      RawImage[String](input).at(Position(2, 0)) shouldBe "c"
      RawImage[String](input).at(Position(2, 1)) shouldBe "e"
    }

  }

}



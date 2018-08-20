package partone.chapterfive

import org.scalatest.{FlatSpec, Matchers}

class StreamSpec extends FlatSpec with Matchers {

  "Stream creating" should "create new empty stream" in {
    Stream().headOption shouldBe None
  }

  it should "create new non empty stream" in {
    Stream(1,2,3).headOption shouldBe Some(1)
  }

  "Stream to list" should "return empty list for Empty Stream" in {
    Stream().toList shouldBe List()
  }

  it should "return list 1 2 3 for Stream 1 2 3" in {
    Stream(1,2,3).toList shouldBe List(1,2,3)
  }

  "Stream - take" should "return 1 2 for take 2 on List 123" in {
    Stream(1,2,3).take(2).toList shouldBe List(1,2)
  }

  it should "return empty list for take 2 on empty list" in {
    Stream().take(2).toList shouldBe List()
  }

  "Stream - drop" should "return 3 for drop 2 on List 123" in {
    Stream(1,2,3).drop(2).toList shouldBe List(3)
  }

  it should "return empty list for drop 2 on empty list" in {
    Stream().take(2).toList shouldBe List()
  }

  "Stream - takeWhile" should "return stream 1 2 3 with takeWhile numbers that - 3 are even or less than 0" in {
    Stream(1,2,3,4).takeWhile(_ - 3 <= 0).toList shouldBe List(1,2,3)
  }

  it should "return empty list for empty stream" in {
    Stream(1).drop(1).takeWhile(_%2 != 0).toList shouldBe List()
  }

}

package partone.chapterfive

import org.scalatest.{FlatSpec, Matchers}

class StreamSpec extends FlatSpec with Matchers {

  "Stream" should "create new empty stream" in {
    Stream().headOption shouldBe None
  }

  "Stream" should "create new non empty stream" in {
    Stream(1,2,3).headOption shouldBe Some(1)
  }

}

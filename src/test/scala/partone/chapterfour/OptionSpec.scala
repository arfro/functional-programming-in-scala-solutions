package partone.chapterfour

import org.scalatest.{FlatSpec, Matchers}

class OptionSpec extends FlatSpec with Matchers {

  "Option map" should "return Option(4) for Option(2) with double fcnt" in {
    Some(2).map(_*2) shouldBe Some(4)
  }
  "Option getOrElse" should "return 4 for None" in {
    None.getOrElse(4) shouldBe 4
  }
  it should "return 2 for Some(2)" in {
    Some(2).getOrElse(4) shouldBe 2
  }
  "Option flatMap" should "return Some(2) for Some(Some(2))" in {
    Some(2).flatMap(Some(_)) shouldBe Some(2)
  }
  it should "return None for None" in {
    None.flatMap(Some(_)) shouldBe None
  }
  "Option orElse" should "return Some(12) for None" in {
    None.orElse(Some(12)) shouldBe Some(12)
  }
  it should "return Some(12) for Some(12)" in {
    Some(12).orElse(Some(1)) shouldBe Some(12)
  }
  "Option filter" should "return Some(12) for fctn _/2 == 6" in {
    Some(12).filter(_/2 == 6) shouldBe Some(12)
  }
  it should "return None for fctn _/3 == 6" in {
    Some(12).filter(_/3 == 6) shouldBe None
  }

  "Option mean" should "return None for Nil" in {
    Option.mean(Nil) shouldBe None
  }
  it should "return 2 for 2 2 2 " in {
    Option.mean(List(2,2,2)) shouldBe Some(2)
  }
  "Option variance" should "return None for Nil" in {
    Option.variance(Nil) shouldBe None
  }
  it should "return 2  for  5 5 5" in {
    Option.variance(List(5, 5, 8)) shouldBe Some(2)
  }

}

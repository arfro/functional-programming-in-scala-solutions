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

  "Option map2" should "return Some(4) for Some(1), Some(3)" in {
    Option.map2(Some(3), Some(3))(fct) shouldBe Some(4)
  }

  it should "return None for None, Some(3)" in {
    Option.map2(None, Some(3))(fct) shouldBe None
  }

  "Option sequence" should "return Some(4) for Some(1), Some(3)" in {
    Option.sequence(List(Some(3), Some(32))) shouldBe Some(List(3, 32))
  }

  "Option sequence_1" should "return Some(4) for Some(1), Some(3)" in {
    Option.sequence(List(Some(3), Some(32))) shouldBe Some(List(3, 32))
  }

  "Option traverse" should "return None for 6 32" in {
    Option.traverse(List(6, 32))(failLargerThan10) shouldBe None
  }

  it should "return Some(30, 25) for 30 25 " in {
    Option.traverse(List(30, 25))(failLargerThan10) shouldBe Some(List(30, 25))
  }

  def fct(a: Int, b: Int): Int = a + b
  def failLargerThan10(a: Int): Option[Int] =
    if(a < 10) None
    else Some(a)
}

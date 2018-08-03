package partone.chapterfour

import org.scalatest.{FlatSpec, Matchers}

class EitherSpec extends FlatSpec with Matchers {

  "Either map - Left" should "return Left(\"abc\") for Left(\"abc\")" in {
    Left("abc").map(fcMap) shouldBe Left("abc")
  }

  it should "return Right(\"abc\") for Right(\"abc\")" in {
    Right("abc").map(fcMap) shouldBe Right("abc")
  }

  "Either flatMap - Right" should "return Right(\"abc\") for Right(\"abc\")" in {
    Right("abc").flatMap(fcMap) shouldBe Right("abc")
  }

  "Either orElse" should "return Right(\"abc\") for Right(\"abc\")" in {
    Right("abc").orElse(fcOrElseOK) shouldBe Right("abc")
  }

  "Either orElse" should "return Right(\"actually.. its ok\") for Left(\"error\")" in {
    Left("error").orElse(fcOrElseOK) shouldBe Right("actually.. its ok")
  }

  "Either map2" should "return Left(\"error\") for Left(\"error\") & Right(\"a\")" in {
    Left("error").map2(Right("a"))(fctMap2) shouldBe Left("error")
  }

  it should "return Right(\"ab\") for Right(\"a\") & Right(\"b\")" in {
    Right("a").map2(Right("b"))(fctMap2) shouldBe Right("ab")
  }


  def fcOrElseOK: Either[String, String] = Right("actually.. its ok")

  def fcMap(a: String) : Either[String, String] = {
    a match {
      case "" => Left("error")
      case as => Right(as)
    }
  }
  def fctMap2(a: String, b: String): String = a + b

}

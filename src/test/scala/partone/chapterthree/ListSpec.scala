package partone.chapterthree

import org.scalatest.{FlatSpec, Matchers}

class ListSpec extends FlatSpec with Matchers {


  "List remove head" should "return Nil for empty list" in {
    CustomList.removeTail(Nil) shouldBe Nil
  }

  "List remove head" should "return Nil for one element list" in {
    CustomList.removeTail(Cons(1, Nil)) shouldBe Nil
  }

  "List remove head" should "return 2 for Cons 1, 2" in {
    CustomList.removeTail(Cons(1, Cons(2, Nil))) shouldBe Cons(2, Nil)
  }

}
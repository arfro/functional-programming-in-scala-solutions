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

  "List setHead" should "replace head with 3" in {
    CustomList.setHead(3, Cons(1, Cons(3, Nil))) shouldBe Cons(3, Cons(3, Nil))
  }

  "List drop" should "drop first two elements" in {
    CustomList.drop(Cons(1, Cons(2, Cons(3, Cons(4, Nil)))), 2) shouldBe Cons(3, Cons(4, Nil))
  }

  "List drop" should "drop first element" in {
    CustomList.drop(Cons(1, Cons(2, Cons(3, Cons(4, Nil)))), 1) shouldBe Cons(2, Cons(3, Cons(4, Nil)))
  }

  "List drop" should "handle empty list" in {
    CustomList.drop(Nil, 3) shouldBe Nil
  }

  "List drop" should "drop all if drop number larger than list length" in {
    CustomList.drop(Cons(1, Cons(2, Cons(3, Cons(4, Nil)))), 9) shouldBe Nil
  }

  "List drop" should "not drop anything is drop number is zero" in {
    CustomList.drop(Cons(1, Cons(2, Cons(3, Cons(4, Nil)))), 0) shouldBe Cons(1, Cons(2, Cons(3, Cons(4, Nil))))
  }

  "List dropWhile" should "drop all first 4s" in {
    // if the function dropWhile wasn't curried then we would have to specify x type
    CustomList.dropWhile(Cons(4, Cons(4, Cons(3, Cons(4, Nil)))))(x => x == 4 ) shouldBe Cons(3, Cons(4, Nil))
  }

  "List length" should "return 4 for size 4" in {
    // if the function dropWhile wasn't curried then we would have to specify x type
    CustomList.lengthFoldRight(Cons(4, Cons(4, Cons(3, Cons(4, Nil))))) shouldBe 4
  }

  "List length" should "return 0 for size 0" in {
    // if the function dropWhile wasn't curried then we would have to specify x type
    CustomList.lengthFoldRight(Nil) shouldBe 0
  }
}
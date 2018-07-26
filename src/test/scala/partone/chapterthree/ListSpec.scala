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

  "List length - fold right" should "return 4 for size 4" in {
    // if the function dropWhile wasn't curried then we would have to specify x type
    CustomList.lengthFoldRight(Cons(4, Cons(4, Cons(3, Cons(4, Nil))))) shouldBe 4
  }

  "List length - folf right" should "return 0 for size 0" in {
    // if the function dropWhile wasn't curried then we would have to specify x type
    CustomList.lengthFoldRight(Nil) shouldBe 0
  }

  "List length - fold left" should "return 4 for size 4" in {
    // if the function dropWhile wasn't curried then we would have to specify x type
    CustomList.lengthFoldLeft(Cons(4, Cons(4, Cons(3, Cons(4, Nil))))) shouldBe 4
  }

  "List length - fold left" should "return 0 for size 0" in {
    // if the function dropWhile wasn't curried then we would have to specify x type
    CustomList.lengthFoldLeft(Nil) shouldBe 0
  }

  "List reverse - fold left" should "returns 4 3 2 1 for 1 2 3 4" in {
    // if the function dropWhile wasn't curried then we would have to specify x type
    CustomList.listReverse((Cons(1, Cons(2, Cons(3, Cons(4, Nil)))))) shouldBe (Cons(4, Cons(3, Cons(2, Cons(1, Nil)))))
  }

  "List reverse - fold left" should "returns empty list for emtpty list" in {
    // if the function dropWhile wasn't curried then we would have to specify x type
    CustomList.listReverse(Nil) shouldBe Nil
  }

  "List append - fold left" should "returns 1 2 3 4 5 for 1 2 3 4" in {
    // if the function dropWhile wasn't curried then we would have to specify x type
    CustomList.append((Cons(1, Cons(2, Cons(3, Cons(4, Nil))))), Cons(5, Nil)) shouldBe (Cons(1, Cons(2, Cons(3, Cons(4, Cons(5, Nil))))))
  }


  "Two lists append - fold left" should "returns 1 2 3 4 for 1 2 & 3 4" in {
    // if the function dropWhile wasn't curried then we would have to specify x type
    CustomList.append((Cons(1, Cons(2, Nil))), Cons(3, Cons(4, Nil))) shouldBe (Cons(1, Cons(2, Cons(3, Cons(4, Nil)))))
  }

  "Add one function" should "returns 2 3 4 5 for 1 2 3 4" in {
    // if the function dropWhile wasn't curried then we would have to specify x type
    CustomList.addOne(Cons(1, Cons(2, Cons(3, Cons(4, Nil))))) shouldBe (Cons(2, Cons(3, Cons(4, Cons(5, Nil)))))
  }

  "Double to string function" should "returns 1 2 3 4 as strings for 1 2 3 4 in ints" in {
    // if the function dropWhile wasn't curried then we would have to specify x type
    CustomList.doubleToString(Cons(1.0, Cons(2.0, Cons(3.0, Cons(4.0, Nil))))) shouldBe (Cons("1.0", Cons("2.0", Cons("3.0", Cons("4.0", Nil)))))
  }

  "Map function" should "returns 1 2 3 4 doubled" in {
    // if the function dropWhile wasn't curried then we would have to specify x type
    CustomList.map(Cons(1.0, Cons(2.0, Cons(3.0, Cons(4.0, Nil)))))(_.toString) shouldBe (Cons("1.0", Cons("2.0", Cons("3.0", Cons("4.0", Nil)))))
  }

  "Map function" should "returns 1 2 3 4 doubled" in {
    // if the function dropWhile wasn't curried then we would have to specify x type
    CustomList.map(Cons(1.0, Cons(2.0, Cons(3.0, Cons(4.0, Nil)))))(_*3) shouldBe (Cons(3.0, Cons(6.0, Cons(9.0, Cons(12.0, Nil)))))
  }
  
}
package partone.chaptertwo

import org.scalatest._

// exercise 2.2

class IsSortedSpec extends FlatSpec with Matchers {

  "IsSortedSpec function" should "return true if sorted for string" in {
    def less(a: String, b: String) = a.length < b.length
    IsSorted.isSorted(Array("1", "23", "dasa"), less) shouldBe true
  }

  it should "calculate correctly for int" in {
    def less(a: Int, b: Int) = a < b
    IsSorted.isSorted(Array(1, 2, 3, 4, 5), less) shouldBe true
  }

  it should "return false if not sorted" in {
    def less(a: Int, b: Int) = a < b
    IsSorted.isSorted(Array(1, 2, 32, 4, 5), less) shouldBe false
  }

  it should "return true for empty" in {
    def less(a: Int, b: Int) = a < b
    IsSorted.isSorted(Array(), less) shouldBe true
  }

}
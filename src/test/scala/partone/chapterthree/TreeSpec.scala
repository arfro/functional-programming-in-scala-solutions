package partone.chapterthree

import org.scalatest.{FlatSpec, Matchers}

class TreeSpec extends FlatSpec with Matchers {

  val tree2Elem = Branch(left = Leaf(44), right = Leaf(3))
  val tree3Elem = Branch(left = Branch(left = Leaf(4), right = Leaf(33)), right = Leaf(3))

  "Tree size" should "return 2 for two elems in a tree" in {
    Tree.size(tree2Elem) shouldBe 2
  }

  "Tree size" should "return 3 for 3 elems in a tree" in {
    Tree.size(tree3Elem) shouldBe 3
  }

}
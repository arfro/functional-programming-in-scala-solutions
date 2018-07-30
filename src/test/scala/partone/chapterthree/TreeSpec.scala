package partone.chapterthree

import org.scalatest.{FlatSpec, Matchers}

class TreeSpec extends FlatSpec with Matchers {

  val tree2Elem = Branch(left = Leaf(44), right = Leaf(3))
  val tree6Elem = Branch(left = Branch(
                                left = Leaf(4),
                                right = Branch(
                                  left = Branch(
                                    left = Leaf(2),
                                    right = Leaf(33)
                                  ),
                                  right = Branch(
                                    left = Leaf(3),
                                    right = Leaf(2)
                                  )
                                )),
                        right = Leaf(3))

  "Tree size" should "return 2 for two elems in a tree" in {
    Tree.size(tree2Elem) shouldBe 2
  }

  it should "return 6 for 6 elems in a tree" in {
    Tree.size(tree6Elem) shouldBe 6
  }

  "Tree maximum" should "return 33 for 4 2 33 3 2" in {
    Tree.maximum(tree6Elem) shouldBe 33
  }

  "Tree depth longest" should "return 33 for 4 3 33" in {
    Tree.depth(tree6Elem) shouldBe 4
  }

}
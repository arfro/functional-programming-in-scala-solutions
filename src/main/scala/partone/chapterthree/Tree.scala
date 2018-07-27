package partone.chapterthree

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree{
  //exercise 3.25
  def size[A](tree: Tree[A]): Int = {
    def traverse(tree: Tree[A], acc: Int): Int = {
      tree match {
        case _: Leaf[A]   => acc + 1
        case branch: Branch[A] => traverse(branch.left, acc) + traverse(branch.right, acc)
      }
    }
    traverse(tree, 0)
  }

}
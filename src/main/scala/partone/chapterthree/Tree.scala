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


  //exercise 3.26
  def maximum(tree: Tree[Int]): Int = {
    tree match {
      case Leaf(leaf) => leaf
      case Branch(left, right) => maximum(left) max maximum(right)
    }
  }

  //exercise 3.27
  def depth[A](tree: Tree[A]): Int = {
    def getLongest(tree: Tree[A], pathDist: Int, maxDist: Int): Int = {
      tree match {
        case Leaf(_) => pathDist
        case Branch(left, right) =>
          getLongest(left, pathDist + 1, if(pathDist > maxDist) pathDist else maxDist)  max
          getLongest(right, pathDist + 1, if(pathDist > maxDist) pathDist else maxDist)
      }
    }
    getLongest(tree, 0, 0)
  }

}
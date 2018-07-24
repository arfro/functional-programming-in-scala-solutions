package partone.chapterthree

sealed trait CustomList[+A]
case object Nil extends CustomList[Nothing]
case class Cons[+A](head: A, tail: CustomList[A]) extends CustomList[A]

object CustomList{

  //exercise 3.2
  def removeTail[A](list: CustomList[A]): CustomList[A] = {
    list match {
      case Nil => Nil
      case Cons(_, tail) => tail
    }
  }
}
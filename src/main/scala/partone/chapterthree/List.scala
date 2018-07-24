package partone.chapterthree

sealed trait CustomList[+A]
case object Nil extends CustomList[Nothing]
case class Cons[+A](head: A, tail: CustomList[A]) extends CustomList[A]

object CustomList{

  def apply[A](as: A*): CustomList[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  //exercise 3.2
  def removeTail[A](list: CustomList[A]): CustomList[A] = {
    list match {
      case Nil => Nil
      case Cons(_, tail) => tail
    }
  }

  //exercise 3.3
  def setHead[A](head: A, list: CustomList[A]): CustomList[A] = {
    list match {
      case Nil => Nil
      case Cons(_, tail) => Cons(head, tail)
    }
  }

  //exercise 3.4
  def drop[A](list: CustomList[A], n: Int): CustomList[A] = {
    if (n == 0) list
    else
    list match {
      case Nil => Nil
      case Cons(_, tail) if (n > 1) => drop(removeTail(list), n-1)
      case Cons(_, tail) if (n <= 1) => tail
    }
  }


  //exercise 3.5
  def dropWhile[A](list: CustomList[A])(f: A => Boolean): CustomList[A] = {
      list match {
        case Nil => Nil
        case Cons(head, tail) if (f(head)) => dropWhile(drop(list, 1))(f)
        case Cons(head, tail) => Cons(head, tail)
      }
  }


}
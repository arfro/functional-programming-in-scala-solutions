package partone.chapterthree

import scala.annotation.tailrec

sealed trait CustomList[+A]
case object Nil extends CustomList[Nothing]
case class Cons[+A](head: A, tail: CustomList[A]) extends CustomList[A]

object CustomList {

  def apply[A](as: A*): CustomList[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def foldRight[A, B](as: CustomList[A], z: B)(f: (A, B) => B): B = {
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }
  }

  //exercise 3.10
  def foldLeft[A, B](as: CustomList[A], z: B)(f: (B, A) => B): B = {
    @tailrec
    def loop(as: CustomList[A], accum: B): B = {
      as match {
        case Nil => accum
        case Cons(x, xs) => loop(xs, f(accum, x))
      }
    }
    loop(as, z)
  }

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
        case Cons(_, tail) if n > 1 => drop(removeTail(list), n - 1)
        case Cons(_, tail) if n <= 1 => tail
      }
  }


  //exercise 3.5
  def dropWhile[A](list: CustomList[A])(f: A => Boolean): CustomList[A] = {
    list match {
      case Nil => Nil
      case Cons(head, tail) if f(head) => dropWhile(drop(list, 1))(f)
      case Cons(head, tail) => Cons(head, tail)
    }
  }


  //exercise 3.9
  def lengthFoldRight[A](as: CustomList[A]): Int = {
    foldRight(as, 0)((_, sum) => sum + 1)
  }

  //exercise 3.11
  def lengthFoldLeft[A](as: CustomList[A]): Int = {
    foldLeft(as, 0)((sum, _) => sum + 1)
  }

  //exercise 3.11
  def sum(as: CustomList[Int]): Int = {
    foldLeft(as, 0)(_ + _)
  }

  //exercise 3.11
  def product(as: CustomList[Int]): Int = {
    foldLeft(as, 1)(_ * _)
  }

  //exercise 3.12
  def listReverse[A](as: CustomList[A]): CustomList[A] = {
    foldLeft(as, CustomList[A]())((x, head) => Cons(head, x))
  }

  //exercise 3.14
  def append[A](as: CustomList[A], elem: CustomList[A]): CustomList[A] = {
    foldRight(as, elem)((tail, x) => Cons(tail, x))
  }

  //exercise 3.15
  def concat[A](as1: CustomList[A], as2: CustomList[A]): CustomList[A] = {
    foldLeft(as2, CustomList[A]())((x, head) => append(as1, Cons(head, x)))
  }

  //exercise 3.16
  def addOne(as: CustomList[Int]): CustomList[Int] = {
    foldRight(as, CustomList[Int]())((tail, x) => Cons(tail + 1, x))
  }

  //exercise 3.17
  def doubleToString(as: CustomList[Double]): CustomList[String] = {
    foldRight(as, CustomList[String]())((tail, x) => Cons(tail.toString, x))
  }

  //exercise 3.18
  def map[A, B](as: CustomList[A])(f: A => B): CustomList[B] = {
    foldRight(as, CustomList[B]())((tail, x) => Cons(f(tail), x))
  }

  //exercise 3.19
  def filter[A](as: CustomList[A])(f: A => Boolean): CustomList[A] = {
    foldLeft(as, CustomList[A]())((x, head) =>
      if(f(head)) Cons(head, x)
      else x
    )
  }

  //exercise 3.20
  def flatMap[A, B](as: CustomList[A])(f: A => CustomList[B]): CustomList[B] ={
    as match {
      case Nil => Nil
      case Cons(h, t) => append(f(h), flatMap(t)(f))
    }
  }


}

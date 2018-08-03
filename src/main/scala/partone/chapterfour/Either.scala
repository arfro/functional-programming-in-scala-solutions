package partone.chapterfour

sealed trait Either[+E, +A]{
  //exercise 4.6
  def map[B](f: A => B): Either[E, B] = {
    this match {
      case Left(l) => Left(l)
      case Right(r) => Right(f(r))
    }
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = {
    this match {
      case Left(l) => Left(l)
      case Right(r) => f(r)
    }
  }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = {
    this match {
      case Left(_) => b
      case Right(r) => Right(r)
    }
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C]= {
    for {
      dis <- this
      that <- b
    } yield f(dis, that)
  }

}
case class Left[+E](msg: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]


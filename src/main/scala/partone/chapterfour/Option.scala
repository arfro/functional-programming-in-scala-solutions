package partone.chapterfour

sealed trait Option[+A] {
  //exercise 4.1
  def map[B](f: A => B): Option[B] = this match {
    case Some(something) => Some(f(something))
    case None => None
  }

  def getOrElse[B>:A](default: => B): B = this match {
    case Some(something) => something
    case None => default
  }

  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case Some(something) => f(something)
    case None => None
  }

  def orElse[B>:A](ob: => Option[B]): Option[B] = this match {
    case Some(something) => Some(something)
    case None => ob
  }

  def filter(f: A => Boolean): Option[A] = this match {
    case Some(something) =>
      if(f(something)) Some(something)
      else None
    case None => None
  }
}
case class Some[A](get: A) extends Option[A]
case object None extends Option[Nothing]

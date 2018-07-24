package partone.chaptertwo

// exercise 2.2

object IsSorted {

  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    if (as.size < 2) true
    else {
      as.sliding(2, 1).find(x => !ordered(x(0), x(1))).isEmpty
    }
  }
}
package main.scala.partone

import scala.annotation.tailrec

// exercise 2.1

object Fibonacci {

  def fib(n: Int): Int = {
    @tailrec
    def loop(n : Int, next: Int, acc: Int): Int ={
      if(n == 0) acc
      else loop(n-1, acc + next,next)
    }
    loop(n,1,0)
  }

}
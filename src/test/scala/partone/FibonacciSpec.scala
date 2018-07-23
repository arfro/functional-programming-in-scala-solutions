package test.scala.partone

import main.scala.partone.Fibonacci
import org.scalatest._

// exercise 2.1

class FibonacciSpec extends FlatSpec with Matchers {

  "Fibonacci nr function" should "calculate correctly for 5" in {
    Fibonacci.fib(5) shouldBe 5
  }

  "Fibonacci nr function" should "calculate correctly for 0" in {
    Fibonacci.fib(0) shouldBe 1
  }

}
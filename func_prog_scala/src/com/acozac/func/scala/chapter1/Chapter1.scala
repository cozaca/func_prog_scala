package com.acozac.func.scala.chapter1

object Chapter1 {

  def abs(n: Int): Int = {
    if (n > 0) n else -n
  }

  def factorial(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, acc: Int): Int = {
      if (n <= 0) acc
      else go(n - 1, n * acc)
    }
    go(n, 1)
  }

  def fibonacci(n: Int): Int = {
    def go(n: Int): Int = {
      if (n == 0) 0
      else if (n == 1) 1
      else go(n - 1) + go(n - 2)
    }
    go(n)
  }

  def fibTail(n: Int): Int = {
    @annotation.tailrec
    def go(a: Int, b: Int, c: Int): Int = {
      if (c <= 0) a
      else go(b, a + b, c - 1)
    }
    if (n <= 2) 1
    else go(0, 1, n)
  }

  private def formatAbs(x: Int) = {
    val msg = "The absolute value of %d is %d"
    msg.format(x, abs(x))
  }

  def isSorted[A](ds: Array[A], gt: (A, A) => Boolean): Boolean = {
    @annotation.tailrec
    def go(i: Int, h: A, t: Array[A]): Boolean = {
      if (i - 1 == 0) true
      else if (!gt(h, t.head)) false
      else go(i - 1, t.head, t.tail)
    }
    go(ds.length, ds.head, ds.tail)
  }

  def main(args: Array[String]): Unit = {
    println(formatAbs(-42))
    println(factorial(5))
    println(fibonacci(5))
    println(fibTail(5))
    val ds = Array(9, 8, 7, 1)
    println(isSorted[Int](ds, (x, y) => x > y))
  }
}

import scala.annotation.tailrec

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]
object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x,xs) => x + sum(xs)
  }
  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  // ex 2. implement the function tail for "removing" the
  // first element of a List
  def tail(ds: List[Int]): List[Int] = ds match {
    case Nil => Nil
    case Cons(x, xs) => xs
  }

  // ex 3. Generalize tail to the function drop,
  // which removes the first n elements from a list
  def drop(ds: List[Int], n:Int): List[Int] =
    if(n <= 0 ) ds
    else ds match {
      case Nil => Nil
      case Cons(x, xs) => drop(xs, n-1)
    }

  // ex.4 Implement dropWhile,10 which removes elements from the
  // List prefix as long as they match a predicate.
  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] =
    l match {
      case Nil => Nil
      case Cons(x, xs) if f(x) => dropWhile(xs)(f)
      case r => r
    }

  def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
    case Nil => a2
    case Cons(h, t) => Cons(h, append(t, a2))
  }

  //ex.5 Using the same idea, implement the function setHead for
  // replacing the first element of a List with a different value
  def setHead(ds: List[Int], v : Int) : List[Int] = ds match {
    case Nil => Nil
    case Cons(x, xs) => Cons(v, xs)
  }

  // ex. 6 Implement a function,
  // init, which returns a List consisting
  //of all but the last element of a List
  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(x, Nil) => Nil
    case Cons(x, xs) => Cons(x, init(xs))
  }


  def foldRight[A, B](l:List[A], e:B)(f:(A,B) => B):B = l match {
    case Nil => e
    case Cons(x,xs) =>f(x,foldRight(xs,e)(f))
  }

  @tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z,x))(f)
  }

  def length[A](l:List[A]): Int = l match {
    case Nil =>0
    case Cons(x,xs) => 1 +length(xs)
  }

  // ex. 11 Compute the length of a list using foldRight
  def lengthRight[A](as: List[A]): Int = foldRight(as, 0)((_, b) =>  b + 1)

  // ex. 12 Write a function that returns the reverse of a list
  def reverse[A](as:List[A]): List[A] = {
    def iter(as:List[A], acc:List[A]): List[A] = as match {
      case Nil => acc
      case Cons(h,t) => iter(t, Cons(h, acc))
    }
    iter(as, Nil)
  }

  def reverseFold[A](as:List[A]): List[A] = foldLeft(as, List[A]())((b,a) => Cons(a,b))

  def foldLeft2[A,B](as:List[A], z:B)(f:(B,A) => B): B = foldRight(as, (b:B) => b)((a,g) => b=> g(f(b,a)))(z)

  // ex.13 foldLeft in terms of foldRight
  def foldLeftViaFoldRight[A,B](l: List[A], z: B)(f: (B,A) => B): B =
    foldRight(l, (b:B) => b)((a,g) =>
      b =>
        g(f(b,a)))(z)

  // ex. 14 Implement append in terms of either foldLeft
  def appendFoldR[A](a1:List[A], a2:List[A]): List[A] = {
    foldRight(a1, a2)((a, b) => Cons(a,b))
  }

  // ex. 14 Implement append in terms of either foldLeft
  def appendFoldL[A](a1:List[A], a2:List[A]): List[A] = {
    foldLeft(reverse(a1), a2)((b, a) => Cons(a,b))
  }

  // ex. 16 Write a function that transforms a list of integers by adding 1
  // to each element
  def transformInt(l:List[Int]): List[Int] =
    foldRight(l, List[Int]())((a:Int,b:List[Int]) => Cons(a + 1,b))

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
}
val example = Cons(1, Cons(2, Cons(3, Nil)))
val example2 = List(1,2,3)
val total = List.sum(example)
val tailList = List.tail(List(1,2,3,4))
val dropL = List.drop(List(1,2,3,4,5), 4)
List.setHead(List(1,2,3,4,5), 9)
List.append(List(1,2,3), List(4,5,6))
List.init(List(1,2,3,4))
List.foldRight(List(1.0,2.0,3.0,4.0),0.0)(_+_)
List.foldRight(List(1.0,2.0,0.0,4.0),1.0)(_ * _)
List.foldRight(List("1.0","2.0"),"")(_ + _)
List.foldLeft(List(1,2,3,4,4), 0)(_+_)
List.foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_))
List.length(List(1,2,3))
List.lengthRight(List(1,2,3,4,4))
List.lengthRight(List(1,2,3,4,4))
List.reverse(List(1,2,3,4))
List.reverseFold(List(1,2,3,4))

List.transformInt(List(1,2,3,4))

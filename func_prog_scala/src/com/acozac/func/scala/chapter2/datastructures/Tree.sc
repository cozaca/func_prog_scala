sealed trait Tree[+A]
case class Leaf[A](value:A) extends Tree[A]
case class Branch[A](left:Tree[A], right:Tree[A]) extends Tree[A]

object Tree {

  // ex. 25 Write a function size that counts the number of nodes
  def size(t:Tree[Int]):Int = t match {
    case Leaf(_) => 1
    case Branch(l,r) => size(l) + size(r)
  }

  // ex. 26 Write a function maximum that returns the maximum element
  def max(t:Tree[Int]):Int = t match {
    case Leaf(x) => x
    case Branch(l,r) => max(l).max(max(r))
  }

  // ex. 27 Write a function depth that returns the maximum path length
  // from the root of a tree to any leaf.
  def depth(t:Tree[Int]):Int = t match {
    case Leaf(_) => 1
    case Branch(l,r) => (1+ depth(l)).max(1 + depth(r))
  }

  // ex. 28 Write a function depth that returns the maximum path length
  // from the root of a tree to any leaf.
  def map[A, B](t:Tree[A])(f:A=>B): Tree[B] = t match {
    case Leaf(x) => Leaf(f(x))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  // ex. 29 Generalize ,size ,max , and depth , writing a newsize maximum depth map
  // function that abstracts over their similarities
  // there are not finished yet
  def fold2[A, B](tree:Tree[A], z:B)(f: (A, B) => B): B = tree match
  {
    case Leaf(x) => f(x,z)
    case Branch(left, right) => fold2(left, fold2(right,z)(f))(f)
  }

  def fold[A,B](t: Tree[A], f: A => B)(g: (B,B) => B): B = t match {
    case Leaf(a) => f(a)
    case Branch(l,r) => g(fold(l, f)(g), fold(r, f)(g))
  }

}
val tree = Branch(Branch(Leaf(4), Branch(Leaf(8), Leaf(10))),
  Branch(Branch(Leaf(55), Leaf(21)), Leaf(31)))

Tree.size(tree)

Tree.max(tree)

Tree.depth(tree)

Tree.map(tree)(x => x+1)

Tree.fold2(tree, 0)((_, y)=> y+1)
Tree.fold2(tree, 0)((x,y)=>x.max(y))
Tree.fold[Int, Int](tree, a => 0)((ml, mr) => 1 + ml.max(mr))

Tree.fold[Int, Int](tree, a => 1)((ml, mr) =>  mr + ml )
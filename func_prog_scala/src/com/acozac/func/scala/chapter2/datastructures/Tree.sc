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

}
val tree = Branch(Branch(Leaf(4), Branch(Leaf(8), Leaf(10))),
  Branch(Branch(Leaf(55), Leaf(21)), Leaf(31)))

Tree.size(tree)

Tree.max(tree)

Tree.depth(tree)

Tree.map(tree)(x => x+1)
val divide = new PartialFunction[Int, Int] {
  def apply(x: Int) = 42 / x
  def isDefinedAt(x: Int) = x != 0
}

divide(41)

val add = (a: Int, b: Int) => a + b
val inc = add(_: Int, 1)
inc(10) // returns 11

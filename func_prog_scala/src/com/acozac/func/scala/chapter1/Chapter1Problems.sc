// exercise 3
// Implement partial1 and write down a concrete usage
// of it. There is only one possible implementation that compiles
def partial1[A,B,C](a: A, f: (A, B) => C): B => C =
  (b: B) => f(a, b)
// concrete use case
def p2 = partial1(2, (a:Int, b:Int) => a+b)
p2(3)

// exercise 4
// Let's look at another example, currying, which converts a
// function of N arguments into a function of one argument that returns another
// function as its result
def curry[A,B,C](f: (A,B) => C): A => (B => C) = (a:A) => (b:B) => f(a,b)
def curryAndPartialApp = curry((x:Int, y:Int) => x+y)
// concrete use case with partial application
val add2 = curryAndPartialApp(2)(_)
val sum = add2(4)
// other example
val simpleCurry = curry((x:Int, y:Int) => x+y)(2)(4)
//EXERCISE 5 (optional): Implement uncurry, which reverses the
//transformation of curry
def uncurry[A,B,C](f: (A,B) => C) : (A,B) => C = (a:A, b:B) => f(a,b)
val uncurryAdd = uncurry((x:Int,y:Int) => x+y)(2,3)
//EXERCISE 6: Implement the higher-order function that composes two
// functions
def compose2[A,B,C](f: B=>C, g: A=>B) : A=>C = (a:A) => f(g(a))

def pow2 = (a:Int) => a*a
def doubled = (a:Int) => a*2

def composeFandG = compose2(pow2, doubled)

val result = composeFandG(5)
// f(g(g)) = f(5*2) = f(10) = pow2(10) = 100
def libComposeFandG = pow2 compose doubled

val result2 = libComposeFandG(5)

val result3 = composeFandG andThen doubled

result3(5)
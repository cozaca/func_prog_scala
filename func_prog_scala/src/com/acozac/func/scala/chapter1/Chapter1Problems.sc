
// partial application functions
def partial1[A,B,C](a: A, f: (A, B) => C): B => C =
  (b: B) => f(a, b)

def f(a:Int, b:Int) :Int = a+b

// concrete use case
def p1 = partial1(_:Int, f)
def p2 = partial1(2, f)
p2(3)

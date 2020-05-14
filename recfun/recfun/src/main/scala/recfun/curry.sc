def mapReduce(f:Int => Int, combine :(Int, Int) => Int, n : Int)(a:Int, b:Int) : Int =
  if(a > b) n else combine(f(a), mapReduce(f,combine, n)(a+1, b))


def fact(n:Int) = mapReduce(x => x,(x,y) => x*y, 1)(1,n)
def sum(a:Int, b:Int) = mapReduce(x => x,(x,y) => x+y, 0)(a,b)


fact(5)
sum(1,5)
//sum(x => x)(1,5)
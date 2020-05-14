def init[T](xs: List[T]): List[T] = xs match {
  case List() => throw new Error("init of empty list")
  case List(x) => List()
  case y :: ys => y :: init(ys)
}


def removeAt(n: Int, xs: List[Int]) : List[Int] = n match {
  case 0 => xs match {
    case List() => xs
    case x :: tail => tail
  }
  case _ => xs match {
    case List() => xs
    case x :: tail => x :: removeAt(n - 1, tail)
  }
}

removeAt(1, List('a', 'b', 'c', 'd')) // List(a, c, d)

def flatten(xs: List[Any]): List[Any] = xs match {
  case List() => List()
  case (x : List[Any]) :: tail =>flatten(x) ::: flatten(tail)
  case x :: tail => x :: flatten(tail)
}

flatten(List(List(1, 1), 2, List(3, List(5, 8))))

def msort[T](xs:List[T])(implicit ord:Ordering[T]):List[T] = {
  val n = xs.length/2
  if(n==0) xs
  else {
    def merge(xs: List[T], ys: List[T]): List[T] = (xs, ys) match {
        case (Nil, _) => ys
        case (_, Nil) => xs
        case (x::xs1, y::ys1) =>
          if(ord.lt(x,y)) x::merge(xs1, ys)
          else y::merge(xs, ys1)
      }
    val (fst, scd) = xs splitAt n
    merge(msort(fst), msort(scd))
  }
}

val nums = List(-7, 3, 5, -2, -10, 12, 4, 1,1,6)
msort(nums)

val fruits = List("apple", "pineapple", "banana", "orange")
msort(fruits)

def squareList2(xs: List[Int]): List[Int] =
  xs match {
    case Nil => List()
    case y :: ys => y*y :: squareList2(ys)
  }

def squareList(xs: List[Int]): List[Int] =
  xs map (x => x*x)

def pack[T](xs: List[T]): List[List[T]] = xs match {
  case Nil => Nil
  case x :: xs1 =>
    val (first, rest) = xs span (el=> el==x )
    first :: pack(rest)
}
val value = List("a", "a", "a", "b", "c", "c", "a")
pack(value)

def encode[T](xs:List[T]):List[(T, Int)] = xs match {
  case Nil => Nil
  case x :: xs1 => pack(xs).map(el => (el.head, el.length))
}
encode(value)

def mapFun[T, U](xs: List[T], f: T => U): List[U] =
  (xs foldRight List[U]())( (x,z) => f(x) :: z )

def lengthFun[T](xs: List[T]): Int =
  (xs foldRight 0)( (_, z) => z+1 )

lengthFun(value)
import java.util.NoSuchElementException

trait List[+T] {
  def isEmpty:Boolean
  def head: T
  def tail: List[T]
  def nth(n: Int) : T
  def prepend[U >: T](elem : U ) : List[U] = new Cons[U](elem, this)
}

object Nil extends List[Nothing]{
  override def isEmpty = true

  override def head = throw new NoSuchElementException("Nil.heda")

  override def tail = throw new NoSuchElementException("Nil.heda")

  override def nth(n: Int) = throw new NoSuchElementException("Nil.heda")
}

class Cons[T](val head:T, val tail:List[T]) extends List[T]{
  override def isEmpty = false

  override def nth(n: Int) = {
    if(n == 0) head
    else tail.nth(n-1)
  }
}

def nth[T](n:Int, list:List[T]):T = list.nth(n)


new Cons[Int](5, new Cons[Int](4, new Cons[Int](7, Nil))).nth(0)
new Cons[Int](5, new Cons[Int](4, new Cons[Int](7, Nil))).nth(1)
new Cons[Int](5, new Cons[Int](4, new Cons[Int](7, Nil))).nth(2)
//new Cons[Int](5, new Cons[Int](4, new Cons[Int](7, Nil))).nth(3)
//new Cons[Int](5, new Cons[Int](4, new Cons[Int](7, Nil))).nth(-1)

object List {
  def apply[T](x: T, y:T) : List[T] = new Cons[T](x, new Cons[T](y, Nil))
  def apply[T](x:T) : List[T] = new Cons[T](x, Nil)
  def apply[T]() :List[T] = Nil
}

List(2,3)
List(2)
List()

val x :List[String] = Nil


//def init[T](xs: List[T]): List[T] = xs match {
//  case List() => throw new Error("init of empty list")
//  case List(x) => List()
//  case y :: ys => y :: init(ys)
//}

//def conca1[T](xs: List[T], ys:List[T])= xs match {
//  case List() => ys
//  case x :: tail => x :: conca1(tail, ys)
//}


}
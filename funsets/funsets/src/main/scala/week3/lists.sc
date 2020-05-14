import java.util.NoSuchElementException

trait List[T] {
  def isEmpty:Boolean
  def head: T
  def tail: List[T]
  def nth(n: Int) : T
}

class Nil[T] extends List[T]{
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


new Cons[Int](5, new Cons[Int](4, new Cons[Int](7, new Nil[Int]))).nth(0)
new Cons[Int](5, new Cons[Int](4, new Cons[Int](7, new Nil[Int]))).nth(1)
new Cons[Int](5, new Cons[Int](4, new Cons[Int](7, new Nil[Int]))).nth(2)
new Cons[Int](5, new Cons[Int](4, new Cons[Int](7, new Nil[Int]))).nth(3)
new Cons[Int](5, new Cons[Int](4, new Cons[Int](7, new Nil[Int]))).nth(-1)
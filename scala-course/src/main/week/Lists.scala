object Lists extends App {

  def sort(x:List[Int]):List[Int] = x match {
    case List() => List()
    case y::ys => insert(y, sort(ys))
  }

  def insert(x:Int, xs:List[Int]):List[Int] = xs match {
    case List() =>List(x)
    case y::ys =>
      if(x<=y) x::xs
      else y::insert(x, ys)
  }

  println(sort(List(7,3,2,9,2)))
}

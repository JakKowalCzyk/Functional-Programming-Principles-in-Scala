trait Expr{}

case class Number(n : Int) extends Expr{}

case class Sum(e1:Expr, e2:Expr) extends Expr{}

case class Prod(e1:Expr, e2:Expr) extends Expr{}

case class Var(name:String) extends Expr{}

object Expressions extends App {
  def show(e:Expr):String = e match {
    case Number(n) => n.toString
    case Var(x) => x.toString
    case Sum(e1, e2) => show(e1) + "+" + show(e2)
    case Prod(e1, e2) => show(e1) + " * " + show(e2)
    case Prod(Sum(e1,e2), e3) => show(Sum(e1,e2)) + " * " + show(e3)
    case Prod(e1, Sum(e2,e3)) => show(e1) + " * " + show(Sum(e2,e3))
  }
  println(  show(Sum(Number(1), Number(44))))
  println(show(Sum(Prod(Number(2), Var("x")), Var("y"))))
  println(show(Prod(Sum(Number(2), Var("x")), Var("y"))))
}

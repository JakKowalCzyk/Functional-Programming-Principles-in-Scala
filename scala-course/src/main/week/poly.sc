class Poly(terms0: Map[Int, Double]){
  def this(bindings: (Int, Double)*) = this(bindings.toMap)

  val terms  = terms0 withDefaultValue(0.0)
//  def + (other:Poly) = new Poly(terms ++ (other.terms map adjust))
  def + (other:Poly) = new Poly(other.terms foldLeft terms)(addTerm))

  override def toString: String =
    (for ((exp, coef) <- terms.toList.sorted.reverse) yield coef + "x^" + exp) mkString "+"

  def adjust(term: (Int, Double)): (Int, Double) = {
    val (exp, coef) = term
    exp -> (coef + terms(exp))
  }

  def addTerm(terms: Map[Int, Double], term: (Int, Double)):Map[Int, Double] ={
    val (exp, coef) = term
    terms + (exp -> (coef + terms(exp)))
  }

}

val p1 = new Poly(1->2.0, 3->4.0, 5->6.2)
val p2 = new Poly(0->3.0, 3-> 7.0)
p1+p2

p1.terms.foldLeft()
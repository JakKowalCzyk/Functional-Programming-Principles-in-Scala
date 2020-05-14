abstract class Boolean{
  def ifThenElse[T](t: => T, e : => T) : T

  def &&(x: =>Boolean): Boolean = ifThenElse(x, false)
  def ||(x: =>Boolean): Boolean = ifThenElse(true, x)
  def unary_! : Boolean

  def == (x:Boolean): Boolean = ifThenElse(x, x.unary_!)
  def != (x:Boolean): Boolean = ifThenElse(x.unary_!, x)
}

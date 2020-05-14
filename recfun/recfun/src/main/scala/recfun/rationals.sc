
class Rational(x:Int, y:Int){
  require(y != 0, "denominator must be different then 0")

  def this(x:Int) = this(x,1)

  private def gcd(a:Int, b:Int):Int = if(b==0) a else gcd(b, a %b)

  def numer = x ;
  def denom = y ;

  def common(f : (Int, Int) => Int, that:Rational) :Rational =
    new Rational(f(numer * that.denom,that.numer*denom), denom*that.denom)

  def add(that:Rational) = common((x,y) => x+y, that)

  def neg = new Rational(-numer, denom)

  def sub(that:Rational)=common((x,y) => x-y, that)

  def less(that:Rational) = numer * that.denom < that.numer * denom

  def max(that:Rational) = if(this.less(that)) that else this

  override def toString: String = {
    val g = gcd(numer, denom)
    numer/g + "/" + denom/g
  }
}

val x = new Rational(1, 3)
val y = new Rational(5,7)
val z = new Rational(3,2)
val xy = x.add(y)
xy.neg
y.sub(x)
x.sub(y).sub(z)
y.add(y)
x.less(y)
x.max(y)
//val strange= new Rational(1,0)
//strange.add(strange)
val n = new Rational(5)
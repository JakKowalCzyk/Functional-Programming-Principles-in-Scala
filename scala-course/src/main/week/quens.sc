def isSafe(col: Int, quens: List[Int]) : Boolean = {
  val row = quens.length
  val queensWithRow = (row - 1 to 0 by -1) zip quens
  queensWithRow forall {
    case (r,c) => col != c && math.abs(col-c)!=row-r
  }
}

def quens(n:Int) : Set[List[Int]] = {
  def placeQuens(k:Int):Set[List[Int]] =
    if(k==0) Set(List())
    else
      for {
        quens <- placeQuens(k-1)
        col <- 0 until n
        if isSafe(col, quens)
      } yield col :: quens
  placeQuens(n)
}

def show(quens : List[Int]) = {
  val lines =
    for (col <- quens.reverse)
      yield Vector.fill(quens.length)("* ").updated(col, "X ").mkString
  "\n" + (lines mkString "\n")
}

(quens(8) map show) mkString "\n"
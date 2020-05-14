package recfun

object RecFun extends RecFunInterface {

  def main(args: Array[String]): Unit = {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(s"${pascal(col, row)} ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    def newton(n: Int, k : Int) : Int ={
      if(k == 0 || n==k) 1
      else newton(n-1, k-1)+newton(n-1, k)
    }
    newton(r, c)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def getMissingNumber(chars: List[Char], missing: Int): Int = {
      if (missing < 0) missing
      else if (chars.isEmpty) missing
      else if (chars.head.equals('(')) getMissingNumber(chars.tail, missing + 1)
      else if (chars.head.equals(')')) getMissingNumber(chars.tail, missing-1)
      else getMissingNumber(chars.tail, missing)
    }

    if (chars.isEmpty) true
    else getMissingNumber(chars, 0)== 0
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    def counter(moneyLeft: Int, coins: List[Int]): Int = {
      if (moneyLeft == 0) 1
      else if (moneyLeft > 0 && coins.nonEmpty) counter(moneyLeft - coins.head, coins) + counter(moneyLeft, coins.tail)
      else 0
    }
    if(money == 0 || coins.isEmpty) 0
    else counter(money, coins)
  }
}

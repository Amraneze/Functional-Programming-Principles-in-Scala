package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
    def pascal(c: Int, r: Int): Int = {
      if (c == 0 || c == r) return 1
      pascal(c - 1, r - 1) + pascal(c, r - 1)
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      def balanceAccum(chars: List[Char], openedPar: Int) : Boolean = {
        chars match {
          case Nil => true
          case x::tail => val newOpenedPar =
            if (x == '(') openedPar + 1
            else if (x == ')') openedPar - 1
            else openedPar
            if (newOpenedPar >= 0)
              balanceAccum(tail, newOpenedPar)
            else
              false
          case _ => false
        }
      }
      balanceAccum(chars, 0)
    }

  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      def count(money : Int,  coins: List[Int]): Int = {
        coins match {
          case Nil => 0
          case x::tail => if (x == money) 1
          else if (x > money) 0
          else countChange(money - x, coins) + countChange(money, tail)
        }
      }
      count(money, coins.sorted)
    }
}

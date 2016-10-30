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
      if (c == 0)
        1
      else if (r == c)
        1
      else
        pascal(c-1, r-1) + pascal(c, r - 1)
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {

      def balanceParens(chars: List[Char], open: Int): Boolean = {
        if (open < 0)
          false
        else if (chars.isEmpty && open == 0)
          true
        else if (chars.isEmpty && open != 0)
          false
        else if (chars.head == '(')
          balanceParens(chars.tail, open + 1)
        else if (chars.head == ')')
          balanceParens(chars.tail, open - 1)
        else
          balanceParens(chars.tail, open)
      }

      balanceParens(chars, 0)
    }


  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {

      def countChangeSorted(money: Int, coins: List[Int]): Int = {
        if (money == 0)
          0
        else if (coins.isEmpty)
          0
        else if (money == coins.head)
          1
        else if (money > coins.head)
          countChangeSorted(money, coins.tail) +
          countChangeSorted(money - coins.head, coins)
        else
          0
      }

      countChangeSorted(money, coins.sorted)
    }

    def sum(f: Int => Int) : (Int, Int) => Int = {

      def sumF(a: Int, b: Int): Int = {
        if (a > b) 0
        else f(a) + sumF(a+1, b)
      }

      sumF
    }

    def product(f: Int => Int) : (Int, Int) => Int = {

      def productF(a:Int, b: Int) : Int = {
        if (a > b) 1
        else f(a) * productF(a+1, b)
      }

      productF
    }

    product(x => x)(1, 10)


    def sumProd(f: Int => Int) : (Int, Int, Int, (Int, Int)=>Int) => Int = {

      def sumProdF(a:Int, b: Int, unit: Int, g: (Int, Int) =>Int): Int = {

        if (a > b) unit
        else g(f(a), sumProdF(a+a, b, unit, g))
      }

      sumProdF
    }






}

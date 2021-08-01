package recfun


object RecFun extends RecFunInterface:

  def main(args: Array[String]): Unit =
    println("Pascal's Triangle")
    for row <- 0 to 20 do
      for col <- 0 to row do
        print(s"${pascal(col, row)} ")
      println()

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {

    if (c == 0 || r == 0 || c == r)
      1
    else
      pascal(c, r - 1) + pascal(c - 1, r - 1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {

    def loop(chars: List[Char], stack: Int): Boolean = {
      if (chars.isEmpty)
        stack == 0
      else if (chars.head == '(')
        loop(chars.tail, stack + 1)
      else if (chars.head == ')')
        loop(chars.tail, stack - 1)
      else
        loop(chars.tail, stack)
    }

    if (chars.isEmpty)
      true
    else if (chars.head == chars.last)
      false
    else
      loop(chars, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {

    def loop(money: Int, coins: List[Int]): Int = {
      if (money < 0 || coins.isEmpty )
        0
      else if (money == 0 )
        1
      else
        loop(money, coins.tail) + loop(money - coins.head, coins)
    }

    loop(money, coins)
  }

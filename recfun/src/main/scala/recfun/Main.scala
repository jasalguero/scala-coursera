package recfun
import common._

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
  def pascal(c: Int, r: Int): Int =
    if ((c < 0) || (c > r)) 
      0
    else if (c == 0 && r == 0)
      1
    else 
      pascal(c-1, r-1) + pascal(c, r-1)
    	

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {      
      def loop(chars: List[Char], stack: Int): Boolean =      
	      if (chars.isEmpty)
	        stack == 0
	      else
	        chars.head match {
	        	case '(' => loop(chars.tail, stack +1)
	        	case ')' => if (stack <= 0) false else loop(chars.tail, stack -1)
	        	case _ => loop(chars.tail, stack)
	      	}	        
      loop(chars, 0)
    }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0) 1
    else if (money < 0 ) 0
    else if (coins.isEmpty) 0
    else {
    	countChange(money - coins.head, coins) + countChange(money, coins.tail)
      }
    }
}

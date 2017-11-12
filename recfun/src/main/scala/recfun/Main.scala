package recfun

import java.security.KeyStore.TrustedCertificateEntry

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
  /*Do this exercise by implementing the pascal function in Main.scala, which takes a column c and a row r,
  counting from 0 and returns the number at that spot in the triangle. For example, pascal(0,2)=1,pascal(1,2)=2 and pascal(1,3)=3.
  */
    def pascal(c: Int, r: Int): Int = {
//      def loop(c: Int, r: Int): Int={
        if (c==0 || c==r) 1
        else pascal(c-1,r-1)+pascal(c,r-1)
//      }
    }
  /**
   * Exercise 2
   */
  /*
    chars.isEmpty: Boolean returns whether a list is empty
  chars.head: Char returns the first element of the list
  chars.tail: List[Char] returns the list without the first element
  */
   def balance(chars: List[Char]): Boolean = {

    def loop(subChars: List[Char], parenthesisToClose: Int): Boolean = {
      if (subChars.isEmpty)
        if (parenthesisToClose == 0) true
        else false
      else if (subChars.head == "(".toList(0)) loop(subChars.tail, parenthesisToClose + 1)
      else if (subChars.head == ")".toList(0))
        (if (parenthesisToClose > 0) loop(subChars.tail, parenthesisToClose - 1)
        else false)
      else loop(subChars.tail, parenthesisToClose)
    }
    loop(chars,0)
  }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {

      def loopCoin(coins:List[Int],amount:Int,headCoinCounter:Int):Int={
        if (coins.isEmpty) return 0
        if (coins.head*headCoinCounter==amount) return 1
        if (coins.head*headCoinCounter>amount) return 0
        else
          (loopCoin(coins.tail,amount-coins.head*headCoinCounter,0)
            +loopCoin(coins,amount,headCoinCounter+1)
            )
      }
      loopCoin(coins,money,0)
    }
  }


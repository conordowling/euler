package euler

import scala.math.pow

object OptimumPolynomial {
  
  def apply(function: Int => Int): Int = {
    0
  }
 
  def testFunction(n : Int): Int = {
    n * n * n
  }
  
  def realFunction(n : Int): Int = {
    (0 to 10).map( i => {
      i match {
        case x if x%2==0 => pow(n, x)
        case x if x%2==1 => - pow(n, x)
      }
    }).sum.toInt
  }
}
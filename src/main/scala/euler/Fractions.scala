package euler

import scala.math.BigDecimal
import scala.math.sqrt
import scala.collection.mutable._

case class Fraction( numerator: Int, denominator: Int )

object ContinuedFractions {
  def squareRootFraction(n : Int, length: Int): List[Int] = {
    var f = BigDecimal(sqrt(n))
    var c = List[Int]()
    for( i <- 1 to length ) {
      c = c ::: List( f.toInt )
      f = 1.0 / (f - f.toInt)
    }
    return c
  }
  
  def getPeriod(n : Int ): List[Int] = {
    val series = squareRootFraction( n, 1001 ).slice(1, 100)
    for( l <- 1 to 100) {
      val period = series.slice(0, l)
      var repeating = true
      for(p <- 1 to 5) {
        val check = series.slice( p*l, (p+1)*l )
        if( period.zip(check).filter( x => x._1 != x._2).size > 0 ) {
          repeating = false
        }
      }
      if( repeating ) {
        print( n )
        print(": ")
        println(period)
        return period
      }
    }
    println("No period found")
    return List(-1)
  }
}

object Problem71 {
  
 
  def apply() : Int = {
    var minN = -1
    var maxFrac = BigDecimal(0)
    for( n <- 1 to 500000 ) {
      val d = (n * (7.0/3.0) + 1).toInt
      if(d > 1000000) return minN
      val frac = BigDecimal(n.toFloat) / BigDecimal(d)
      if(  frac > maxFrac && frac < 3.0/7.0 ) {
        maxFrac = frac
        minN = n
      }
    }
    minN
  }
  
  
}

object Problem72 {
  
  def apply(i : Int = 1000000): Int = {
    
    val fractions = Map[Int, Int]()
    var primes = List[Int]()
    for(d <- 2 to i ) {
      var num = d
      for( p <- primes ) {
        if( d % p == 0 ) {
          num = num * (p-1) / p
        }
      }
      if( num == d) {
        primes = primes ::: List(d)
        num = num - 1
      }
      fractions(d) = num
    }
    return fractions.values.sum
  }
  
}

object Problem64 {
  
  def apply(n : Int): Int = {
    var odd = 0
    for( i <- 2 to n ) {
      if( ! (sqrt(i).toInt * sqrt(i).toInt == i) ) {
        val length = ContinuedFractions.getPeriod( i ).length
        if( length % 2 == 1 ) {
          odd = odd + 1
        }
      }
    }
    return odd
  }
  
}
package euler
import scala.math

object ReversibleNumbers {
  
}

object Problem145 {
  
  def naive( limit :Int): Int = {
    var total = 0
    for( i <- 1 until limit ) {
      if( i % 10 != 0 ) {
        val r = reverse(i)
        val s = i + r
        if( oddDigits(s)) {
          println(i)
          total += 1
        }
      }
    }
    return total
  }
  
  def reverse(num : Int): Int = {
    num.toString.reverse.toInt
  }
  
  def oddDigits( num : Int ): Boolean = {
    for( d <- num.toString.map( c => c.toInt) ) {
      if( d % 2 == 0 ) return false
    }
    return true
  }
  
  def apply( maxDigits : Int ) : Int = {
    var total = 0
    for( d <- 2 to maxDigits ) {
      if( d % 2 == 0 ) { // even number of digits
        val firstDigit = 20
        val nDigit = 30
        total += firstDigit * math.pow( nDigit, (d/2)-1 ).toInt
      }else if( d % 4 == 3) { // three digits
        val middleDigit = 5 // 0,1,2,3,4
        
        val endDigits = 20
        val evenDigits = 0
        val oddDigits = 30
        total += middleDigit * endDigits * math.pow(0, (d+1)/4).toInt
      } else { // odd digits greater than three does not work?
        total = total
      }
    }
    total
  }
  
}
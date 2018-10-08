package euler

import scala.math.pow

object Problem63 {
  
  def apply() : Int = {
    var powerfulNumbers = List[Int]()
    for(i <- (1 to 9) ) {
      val j = i / 10.0
      var n = 1
      while( pow(j, n) >= 0.1) {
        powerfulNumbers = powerfulNumbers ::: List( pow(i,n).toInt )
        n += 1
      }
    }
    println(powerfulNumbers)
    powerfulNumbers.length
  }
  
}
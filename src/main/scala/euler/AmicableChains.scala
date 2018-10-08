package euler

object Problem95 {
  def apply(): Int = {
    for(n <- 1 to 1000000) {
      val i = 1
    }
    -1
  }
  
  def properDivisorSum(n : Int ): Int = {
    (1 until n).toList.filter( i => n % i == 0 ).sum
  }
}
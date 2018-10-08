package euler

import scala.collection.mutable._

object Problem44 {
  
  def apply(): Int = {
    //var differencePairs = List()
    var potentialSums = Map[Int, Int]()
    var pentagonals = Map[Int, Boolean]()
    for( k <- 1 to 100000) {
      val pk = getPentagonal(k)
      
      if( potentialSums.getOrElse(pk, -1) > 0) {
        return potentialSums.getOrElse(pk, -1)
      }

      pentagonals += ((pk, true))
      for(j <- 1 to (k-1) ) {
        val pj = getPentagonal(j)
        if( pentagonals.getOrElse(pk - pj, false) )
          potentialSums += ((pk + pj, pk-pj))
      }
    }
    -1
  }
  
  def getPentagonal(n : Int): Int = (n * (3*n -1))/2
  
}
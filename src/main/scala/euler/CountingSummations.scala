package euler

import scala.collection.mutable._

object Problem76 {
  
  def apply(n :Int): Int = {
    var table = Map[(Int,Int),Int](
      ((0,0),1) 
    )
    for(i <- 1 to n) {
      for(largest <- 1 to i ) {
        table((i,largest)) = 
          (1 to largest).toList
            .map(n => table(i-n, List(n,i-n).min))
            .sum
      }
    }
    return table(n,n) - 1
  }
}
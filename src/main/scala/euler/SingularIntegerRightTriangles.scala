package euler

import scala.collection.mutable._
import scala.math

object Problem75 {
  
  def apply() : Int = {
    var lengthMap = Map[Int,Int]().withDefaultValue(0)
    val MAX_LENGTH = 1000000
    for( b <- 2 to MAX_LENGTH/2) {
      val maxA = List( math.sqrt(MAX_LENGTH*MAX_LENGTH - 2*b*b).toInt, b).min
      val minA = List(math.sqrt( (b+1)*(b+1) - b*b).toInt,3).max
      //println(b.toString + " " + minA.toString + " " + maxA.toString)
      for(a <- minA to maxA) {
        val c = math.sqrt( a*a + b*b)
        //println(a,b,c)
        if(c % 1 == 0) {
          if(a+b+c.toInt == 120) println((a,b,c.toInt))
          //println(c)
          lengthMap(a + b + c.toInt) += 1
        }
      }
    }
    println(lengthMap(120))
    lengthMap.toList.filter( t => t._2 == 1 && t._1 <= 1500000).length
  }
  
}
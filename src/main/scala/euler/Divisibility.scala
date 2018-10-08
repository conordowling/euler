package euler

import scala.collection.mutable._

object Divisibility {

  def powerModulo(n :Int, power: Int, modulo: Int): Int = {
    var r = n % modulo
    var map = Map[Int,Int](
        0 -> 1,
        1 -> r
    )
    (2 to power).toList.foreach( i => {
      if(r == 0) return 0
      r = (r * n) % modulo
      if(map.contains(r)) {
        return (map((power-i) % (i - r)) * map(r)) % modulo
      }
      map(i) = r
    })
    return r
  }
  
}

object Problem250 {
  
  def apply(): String = {
    val remainders = (1 to 250250).toList.map( i => Divisibility.powerModulo(i, i, 250) )
    println(remainders)
    ""
  }
}
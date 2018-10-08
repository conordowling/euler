package euler

import scala.collection.mutable._

object Problem92 {
  def apply(bound : Int = 10000000): Int = {
    var terminator = Map[Int,Int](1 -> 1, 89 ->89).withDefaultValue(0)
    for( i <- 1 until bound) {
      var chain = List(i)
      while( terminator( chain.last ) == 0) {
        chain = chain :::  List(squareDigits(chain.last))
      }
      val t = terminator(chain.last)
      chain.foreach( n => terminator(n) = t )
    }
    terminator.values.filter( v => v == 89).size
  }
  def squareDigits(n :Int ) = n.toString.map( c => c.toString.toInt ).map( d => d*d).sum
}
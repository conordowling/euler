package euler

import scala.collection.mutable._
import scala.math

object Problem62 {
  
  def apply(permutations : Int): BigInt = {
    var table = Map[String, List[Int]]().withDefaultValue(List())
    for( n <- 1 to 100000 ) {
      val id = identifier(cube(n))
      val cubes = table(id) ::: List(n)
      if(cubes.size == permutations) { return cube(cubes.min) }
      table(id) = cubes
    }
    return -1
  }
  def identifier(num: BigInt) = { num.toString.map(c => c.toInt).sorted.toString }
  def cube( num: Int) : BigInt = { BigInt(num) * BigInt(num) * BigInt(num) }
}
package euler
import scala.math
import scala.collection.mutable._

object Primes {
  def isPrime(n : Int, primes: List[Int]): Boolean = {
    val bound = math.sqrt(n)
    for(p <- primes) {
      if( n % p == 0 ) {return false}
      if(p > bound) {return true}
    }
    for(i <- (primes.last + 1) to (bound.toInt) ) {
      if( n % i == 0) return false
    }
    return true
  }
}

object Problem51 {
  
  def apply(familySize : Int): Int = {
    var primes = List[Int](2)
    var families = Map[String, List[Int]]().withDefaultValue(List())
    for( n <- 3 to 1000000) {
      if( Primes.isPrime(n, primes) ) {
        primes = primes ::: List(n)
        generateFamilies(n).foreach( f => {
          val family = families(f)
          val updatedFamily = family ::: List(n)
          if( updatedFamily.length == familySize ) {
            return updatedFamily.min
          }
          families(f) = updatedFamily
        })
      }
    }
    -1
  }
  
  def generateFamilies(n : Int) : List[String] = {
    n.toString.distinct.map( digit => replaceDigit(n.toString, digit.toString)).flatten.toList.filter( n => n.contains("*") )
  }
  
  def replaceDigit(number : String, digit : String): List[String] = {
    if( number == "" ) return List("")
    else {
      val rest = replaceDigit( number.slice(1, number.length), digit )
      if( digit == number.slice(0,1) ) {
        return rest.map( r => List( "*" + r, digit + r) ).flatten
      } else {
        return rest.map( r => number.slice(0,1) + r )
      }
    }
  }
  
}

object Problem60 {
  
  def apply(size: Int): Int = {
    var primes = List[Int](2)
    var pairs = Map[(Int,Int),Boolean]().withDefaultValue(false)
    var sets = Map[(Int,Int), List[List[Int]]]().withDefaultValue(List())
    for(n <- 3 to 10000) {
      if( Primes.isPrime(n, primes) ) {
        sets((1,n)) = primes
          .filter(p => Primes.isPrime(concatenate(n,p),primes) && Primes.isPrime(concatenate(p,n),primes))
          .map(p => List(p))
        sets(1,n).foreach( p => { pairs((n,p(0))) = true })
        for( s <- 2 to size-1 ) {
          var setsS = List[List[Int]]()
          for( pair <- sets(1,n).flatten ) {
             for( set <- sets(s-1,pair) ) {
               if( set.forall( p => pairs((n,p)) ) ) {
                 if( s == size - 1 ) { return (set.sum + pair + n )}
                 setsS = setsS ::: List(set ::: List(pair))
               }
             }
          }
          sets((s,n)) = setsS
        }
        primes = primes ::: List(n)
      }
    }
    return -1
  }
 
  def concatenate(a: Int, b: Int): Int = (a.toString + b.toString).toInt 
}

object Problem97 {
  // Could be much faster with efficient exponentiation
  def apply(): String = {
    val k = 28433
    val exponent = 7830457
    
    var n = BigInt(1)
    for(i <- 1 to exponent) {
      n = n * 2
      val n_string = n.toString
      n = BigInt(n_string.slice(n_string.length-10,n_string.length))
    }
    n = n * k
    n = n + 1
    val final_string = n.toString()
    return final_string.slice(final_string.length-10,final_string.length).toString
  }
}
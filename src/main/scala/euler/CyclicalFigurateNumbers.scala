package euler

object Problem61 {
  
  def apply(): Int = {
    val cyclicMaps = functions.map( f => constructCyclicMap(f) )
    val orderings = cyclicMaps.slice(0,5).permutations.map( c => cyclicMaps.slice(5,6) ::: c)
    val solutions = orderings.map( order => {
      order(0).keys.map( start => {
        order.foldLeft(List[(Int,Int)]((start,0)))((s: List[(Int,Int)], m: Map[Int,List[Int]]) => {
          s.map( i => {
            m.getOrElse(i._1, List())
              .map( p => (p, i._1 * 100 + p + i._2) )
          }).flatten
        }).filter(t => t._1 == start)
      }).flatten
    }).flatten.toList
    if(solutions.length == 0) {
      return -1
    } else {
      return solutions(0)._2
    }
    
  }
  
  def triangular(n : Int) = n*(n+1)/2
  def square(n : Int) = n*n
  def pentagonal(n : Int) = n*(3*n-1)/2
  def hexagonal(n : Int) = n*(2*n-1)
  def heptagonal(n : Int) = n*(5*n-3)/2
  def octagonal(n: Int) = n*(3*n-2)
  
  val functions = List( triangular _, square _, pentagonal _, hexagonal _, heptagonal _, octagonal _)
  
  def constructCyclicMap( function : Int=>Int): Map[Int,List[Int]] = {
    (1 to 1000)
      .map( n => function(n) )
      .filter( n => n >= 1000 && n < 10000)
      .map( n => ( n / 100, n % 100 ) )
      .groupBy( n => n._1 )
      .mapValues( nums => nums.map(n => n._2).toList )
  }
}
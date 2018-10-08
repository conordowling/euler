import scala.math
import scala.io.Source

object Problem103 {
  
  def apply(n : Int): String = {
    ""
  }
  
}

object Problem105 {
  
  def apply(): Int = {
    val filename = "data/p105_sets.txt"
    var total = 0
    for( line <- Source.fromFile(filename).getLines ) {
      val nums = line.split(",").map(i => i.toInt)
      println(nums.toList)
      println(nums.sum)
      println( SpecialSubsetSums.isSpecialSubsetSum(nums.toList))
      if( SpecialSubsetSums.isSpecialSubsetSum(nums.toList) ) {
        total += nums.sum
      }
    }
    return total
  }
  
}

object Problem106 {

  def apply(n : Int = 12): Int = {
    numToTest(n)
  }
  
  def numToTest( n : Int ): Int = {
    (4.to(n).by(2)).map( i => (1 to n).combinations(i).length * possibleCombinations(i) ).sum
  }
  
  // How many ways can we divide
  def possibleCombinations( n : Int ): Int = {
    val nums = 1 to n
    nums.combinations(n/2)
      .filter( l => l(0) == 1)
      .map( l => l.zipWithIndex )
      .filter( l => l.exists(p => p._1 >= (p._2 + 1) * 2) )
      .filter( l => l.exists(p => p._1 < (p._2 + 1) * 2) )
      .length
  }
  
}

object SpecialSubsetSums {
  
  def isSpecialSubsetSum(unsortedNums : List[Int] ): Boolean = {
    val nums = unsortedNums.sorted
    // check list for uniqueness and positivity
    if( nums.distinct.filter(n => n > 0).length < nums.length) {
      println("Invalid")
      return false
    }
    
    // If B contains more elements than C, then S(B) > S(C)
    for( i <- (2 to nums.length / 2) ) {
      if( nums.slice(0, i).sum <= nums.slice(nums.length - (i-1), nums.length).sum )
        return false
    }
    // Then to check for subset sums, we only need sets with equal numbers of elements
    for( n <- 2 to nums.length/2 ) {
      for( combo1 <- nums.combinations(n) ) {
        for(combo2 <- nums.filter( i => ! combo1.contains(i) ).combinations(n) ) {
          if( combo1.sum == combo2.sum ) return false
        }
      }
    }
    
    true
  }
  
  def setString( nums: List[Int] ): String = {
    nums.map(n => n.toString).mkString
  }
  
}
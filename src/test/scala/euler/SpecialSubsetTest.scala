package test.scala.euler

import org.scalatest._
import main.scala.euler._

class SpecialSubsetTest extends FlatSpec with Matchers {
  
  "A Problem103 Solver" should "find the optimal set for n=6" in {
    Problem103(6) shouldEqual "111819202225"
  }
  
  "A Problem105 Solver" should "determine whether a set is a special subset" in {
    
  }
  
  "A Problem106 Solver" should "find combinations for n=7" in {
    Problem106(7) shouldEqual 70
  }
  
}
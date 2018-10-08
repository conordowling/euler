package euler

import org.scalatest._
import euler.Problem220

class HeighwayDragonTest extends FlatSpec with Matchers {
  
  "A Problem220 Solver" should "reproduce example" in {
    val bruteForce = Problem220.evaluateString(Problem220.orderN(10), 500)
    val dynamic = Problem220.maxSteps(10, 500)
    bruteForce.x shouldEqual 18
    bruteForce.y shouldEqual 16
    bruteForce.steps shouldEqual 500
    
    dynamic.x shouldEqual 18
    dynamic.y shouldEqual 16
    bruteForce.steps shouldEqual 500
  }
  
}
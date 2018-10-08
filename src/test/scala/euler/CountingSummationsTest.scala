package euler

import org.scalatest._
import euler.Problem76

class CountingSummationsTest extends FlatSpec with Matchers {
  
  "A Problem76 Solver" should "count summations" in {
    Problem76(5) shouldEqual 6
  }
}
package test.scala.euler

import org.scalatest._
import main.scala.euler._

class SpecialSubsetTest extends FlatSpec with Matchers {

  "A Problem60 solver" should "find the solution for sets of size 4" in {
    Problem60(4) shouldEqual 792
  }

}
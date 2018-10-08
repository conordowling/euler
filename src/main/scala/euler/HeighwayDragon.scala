package euler

import scala.collection.mutable._

object Problem220 {
  
  def apply(): String = {
    val state = maxSteps(50, BigInt("1000000000000"))
    state.x.toString + "," + state.y.toString
  }
  
  val a = "aRbFR"
  val b = "LFaLb"
  
  def buildLookupTable(n : Int) = {
    var map = Map[(String, Int), Progress](   )
    for(i <- 1 to n) {
      map += (
          (("F", i), Progress(0,1,0,1)),
          (("R", i), Progress(0,0,1,0)),
          (("L", i), Progress(0,0,3,0))
      )
      
      val aProgress = a
        .map( c => map.getOrElse( (c.toString,i-1), Progress(0,0,0,0)) )
        .foldLeft(Progress(0,0,0,0))((s1, s2) => addStates(s1,s2))
      
      val bProgress = b
        .map( c => map.getOrElse( (c.toString, i-1), Progress(0,0,0,0)) )
        .foldLeft(Progress(0,0,0,0))((s1, s2) => addStates(s1,s2))
      
      map += ((("a",i), aProgress), (("b",i), bProgress))
    }
    map
  }
  
  case class Progress(x : BigInt, y: BigInt, d: Int, steps: BigInt)
  
  def addStates(state1 : Progress, state2: Progress): Progress = {
    val x = state1.d match {
      case 0 => state1.x + state2.x
      case 1 => state1.x + state2.y
      case 2 => state1.x - state2.x
      case 3 => state1.x - state2.y
    }
    
    val y = state1.d match {
      case 0 => state1.y + state2.y
      case 1 => state1.y - state2.x
      case 2 => state1.y - state2.y
      case 3 => state1.y + state2.x
    }
    val d = (state1.d + state2.d) % 4
    val steps = state1.steps + state2.steps
    
    Progress(x,y,d,steps)
  }
  
  def evaluateString(commands : String, maxSteps : Int): Progress = {
    val end = commands.foldLeft(Progress(0,0,0,0))( (p,c) => {
      val x = p.x
      val y = p.y
      val d = p.d
      val s = p.steps
      if(s == maxSteps) return p
      c.toString match {
        case "F" =>
          d match {
            case 0 => Progress(x, y+1, d, s+1)
            case 1 => Progress(x+1, y, d, s+1)
            case 2 => Progress(x, y-1, d, s+1)
            case 3 => Progress(x-1, y, d, s+1)
          }
        case "R" => Progress(x,y,(d+1)%4, s)
        case "L" => Progress(x,y,(d+3)%4, s)
        case _ => Progress(x,y,d,s)
      }
    })
    end
  }
  
  def dynamicEvaluation( n : Int ): Progress = {
    val map = buildLookupTable(n+1)
    addStates( map.getOrElse(("F",n+1), Progress(0,0,0,0)), map.getOrElse(("a",n+1), Progress(0,0,0,0)))
  }
  
  def maxSteps( n : Int, steps : BigInt, string :List[String] = List("F","a") ): Progress = {
    var map = buildLookupTable(n)
    
    var total = 0
    var state = Progress(0,0,0,0)
    for( s <- string ) {
      val nextState = map.getOrElse((s,n), Progress(0,0,0,0) )
      val newState = addStates(state,nextState)
      if( newState.steps == steps ) return newState
      if( newState.steps > steps ) {
        val recursiveString = s match {
          case "a" => a.toList.map(_.toString)
          case "b" => b.toList.map(_.toString)
          case _ => println("FAILURE")
            List("")
        }
        println("recursive call")
        println(state)
        println(n-1)
        println(steps - state.steps)
        println(recursiveString)
        return addStates( state, maxSteps(n-1, steps - state.steps, recursiveString) )
      }
      state = addStates(state,nextState)
        
    }
    state
  }
  
  def rewriteString(Dn : String): String = {
    Dn.map( c => c.toString match {
      case "a" => a
      case "b" => b
      case _ => c.toString
    }).mkString
  }
  
  def orderN(n : Int): String = {
    (1 to n).foldLeft("Fa")((dn, _) => rewriteString(dn) )
  }
  
  
  
}
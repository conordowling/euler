package euler
/*
object Problem94 {
  
  def apply(): Int = {
    -1
  }
  
  def solve(puzzle : Sudoku): Sudoku = {
    puzzle
  }
  
  def deduce( puzzle: Sudoku) : Sudoku = {
    for( x <- 1 to 9 ) {
      for( y <- 1 to 9 ) {
        if( puzzle.grid.getOrElse((x,y),0) == 0 ) {
          val possibilities = getPossibilities((x,y), puzzle)
          if(possibilities.length == 0)
        }
      }
    }
  }
  
  def getPossibilities( coordinate : (Int, Int), puzzle: Sudoku ): List[Int] = {
    val x = coordinate._1
    val y = coordinate._2
    for( z <- 1 to 9 ) {
      
    }
    
    
  }
  
  case class Sudoku( grid : Map[(Int,Int),Int] )
  
}*/
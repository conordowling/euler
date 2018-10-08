package euler

object Squares {
 
  def digitalRoot(digits : List[Int]):Int = {
    digits.sum match {
      case x if x < 10 => return x
      case x if x >= 10 => return digitalRoot( digits.sum.toString.toList.map( c => c.toString.toInt ) )
    }
  }
  
}

object Problem206 {
  
  def apply(): String = {
    val ending = "00"
    
    val existingDigits = List(1,2,3,4,5,6,7,8,9)
    for( num <- 0 until 100000000 ) {
      
    }
    
    
    val number = "111223344556677889"
    return number + ending
  }
  
}
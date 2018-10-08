object PolistPostfixNotation {
  case class Node( value : String, next: Node )
  class Stack {
  	var head: Node = null
  	def push( value : String ) = { head = new Node(value, head) }
  	def pop(): String = {val value = head.value;head = head.next; value } }
  def main( values : Array[String] ): Unit = { val stack = new Stack()
  	for( v <- values ) { v match {
  			case "x" => stack.push( ( (stack.pop().toInt) * (stack.pop().toInt)).toString)
  			case "/" => stack.push( ( (stack.pop().toInt) / (stack.pop().toInt)).toString)
  			case "+" => stack.push( ( (stack.pop().toInt) + (stack.pop().toInt)).toString)
  			case "-" => stack.push( ( (stack.pop().toInt) - (stack.pop().toInt)).toString)
  			case v : String => stack.push( v ) } }
  	println(stack.pop().toInt) } }
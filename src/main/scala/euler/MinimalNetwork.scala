package euler

import scala.io.Source
import scala.collection.mutable._
object Problem107 {
  
  def apply(): Int = {
    val network = readNetwork("data/p107_network.txt")
    val minCost = minimimSpanningTree(network)
    val totalCost = network.values.map( edges => edges.map(_.cost).sum).sum / 2
    totalCost - minCost
  }
  
  def readNetwork( string : String ): Map[Int,List[Edge]] = {
    var matrix = Map[Int, List[Edge]]().withDefaultValue(List())
    var index = 0
    Source.fromFile(string).getLines.foreach( line => {
      val costs = line.split(",")
        .zipWithIndex
        .filter( t => t._1 != "-" )
        .map( t => Edge( t._2, t._1.toInt ) )
      matrix(index) = costs.toList
      index += 1
    })    
    matrix
  }
  
  def minimimSpanningTree( adjacencies : Map[Int,List[Edge]] ): Int = {
    var nodes = List(0)
    var cost = 0
    while(nodes.length < adjacencies.keys.size ) {
      val minEdge = nodes
        .map( node => adjacencies(node) )
        .flatten
        .filter( edge => !nodes.contains(edge.node) )
        .minBy(e => e.cost)
      nodes = minEdge.node :: nodes
      cost += minEdge.cost
    }
    cost
  }
}
  
case class Edge(node: Int, cost: Int)
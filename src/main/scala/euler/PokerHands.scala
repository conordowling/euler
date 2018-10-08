package euler

import scala.io.Source

object Problem54 {
  
  def apply(filename: String) : Int = {
    //val filename = "data/p054_poker.txt"
    var total = 0
    for( line <- Source.fromFile(filename).getLines ) {
      val cards = line.split(" ").map(s => parseCard(s))
      val hand1 = cards.slice(0,5).toList
      val hand2 = cards.slice(5,10).toList
      println("\nHANDS:")
      if(winner(hand1, hand2)) {
        println("player1 wins!")
        total += 1
      }
    }
    total
  }
  
  def parseCard(string : String): Card = {
    val value = string(0).toString match {
      case "T" => 9
      case "J" => 10
      case "Q" => 11
      case "K" => 12
      case "A" => 13
      case _ => string(0).toString.toInt - 1
    }
    val suit = string(1).toString
    Card(suit, value)
  }
  
  def winner(hand1: List[Card], hand2: List[Card] ): Boolean = {
    val score1 = scoreHand(hand1)
    println(score1)
    val score2 = scoreHand(hand2)
    println(score2)

    if( score1 > score2) return true
    if( score2 > score1) return false
    return handValue(hand1, hand2)
  }
  
  def scoreHand( hand: List[Card] ): Double = {
    val values = hand.map(_.value)
    val flush = hand.map(c => c.suit).distinct.length == 1
    val straight = values.distinct == 5 && (hand.map(_.value).max - hand.map(_.value).min ) == 4
    val svals = sortByOccurrence(hand)
    println(svals)
    
    // Royal Flush and Straight Flush
    if( flush && straight ) return 10 + (hand.map(_.value).max / 13.0)
    
    // Four of a kind and full house
    if( svals.length == 2)
      if( svals(0)._2 == 4) return 9 + (svals(0)._1 / 13.0)
      else return 8 + (svals(0)._1  + (svals(1)._1)/13.0)/13.0
    
    // Flush
    if( flush ) return 7
    
    // Straight
    if( straight ) return 6
    
    // Three of A Kind and Two Pair
    if( svals.length == 3)
      if( svals(0)._1 == 3 ) return 5 + (svals(0)._2 /13.0)
      else return 4 + (svals(0)._1  + (svals(1)._1)/13.0)/13.0
    
      // One Pair
    if( svals.length == 4) return 3 + svals(0)._1/13.0
    
    return 0
  }
  
  def handValue( hand1 : List[Card], hand2: List[Card] ): Boolean = {
    val vals1 = sortByOccurrence(hand1) 
    val vals2 = sortByOccurrence(hand2)
    for( i <- 0 until (vals1.length) ) {
      if( vals1(i)._1 > vals2(i)._1 ) return true
      if( vals1(i)._1 < vals2(i)._1 ) return false
    }
    return false
  }
  
  def sortByOccurrence(hand: List[Card]): List[(Int,Int)] = {
    hand.groupBy(_.value).mapValues(_.size).toList.sortBy(c => - (c._2 + c._1/13.0) )
  }
}
  
case class Card(suit: String, value: Int)
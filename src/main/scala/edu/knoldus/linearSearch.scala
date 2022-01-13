package edu.knoldus

import scala.annotation.tailrec


case object linearSearch extends App {
  def bubbleSort(array: Array[Int]): Array[Int] = {
    @tailrec
    def innerBubble(list1: List[Int],finalList: List[Int] = List()): List[Int] ={
      @tailrec
      def innerMostBubble(myList: List[Int] = List(),a: Int = 0): List[Int] ={
        if(a<myList.length-1) {
          if (myList(a+1)<myList(a)) {
            val right = myList.drop(a+2)
            val left = myList.take(a)
            innerMostBubble(left ++ swapNum(myList(a),myList(a + 1)) ++ right, a + 1)
          }
          else {innerMostBubble(myList, a + 1)}
        }
        else {myList}
      }
      def swapNum(a: Int,b: Int): List[Int] = {
        val newList = List(b, a)
        newList
      }
      val passList = innerMostBubble(list1)
      val (left,right)=passList.splitAt(passList.length-1)
      val myFinalList = right ++ finalList
      if (left.lengthIs>0) {innerBubble(left,myFinalList)}
      else {myFinalList}
    }
    innerBubble(array.toList).toArray
  }
    println(bubbleSort(Array(5,4,3,2,1)))


}



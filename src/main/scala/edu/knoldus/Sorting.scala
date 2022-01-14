package edu.knoldus

import scala.annotation.tailrec


class Sorting {


  def insertionSort(array: Array[Int]): Array[Int] = {
    val myList: List[Int] = array.toLis
    def helperInsertionSort(myList: List[Int]): List[Int] =
      if (myList.isEmpty) {Nil}
      else {insert(myList.head, helperInsertionSort(myList.tail))}

    def insert(a: Int, myList: List[Int]): List[Int] =
      if (myList.isEmpty || a <= myList.head) {a :: myList}
      else {myList.head :: insert(a, myList.tail)}

    helperInsertionSort(myList).toArray
  }

  def selectionSort(array: Array[Int]): Array[Int] = {
    @tailrec
    def innerSelection(array: Array[Int],list: List[Int] = List()): Array[Int]={
      val min = array.min
      val list1=list ++ array.toList.filter(_==min)
      if (array.filter(_>min).lengthIs==0) list1.toArray
      else innerSelection(array.filter(_>min),list1)
    }
    innerSelection(array)
  }



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

  println(Array())
}

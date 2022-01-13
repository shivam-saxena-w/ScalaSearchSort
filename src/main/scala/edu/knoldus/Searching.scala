package edu.knoldus

import scala.annotation.tailrec

class Searching {

  def binarySearch(array: Array[Int], elem: Int): Boolean = {
    @tailrec
    def binarySearchRec(array: Array[Int], elem: Int, start: Int, end: Int): Boolean = {
      if (end <= start) false else {
        val mid = start + end / 2
        if (array(mid) == elem) true
        else if (array(mid) > elem) binarySearchRec(array, elem, start, mid)
        else binarySearchRec(array, elem, mid + 1, end)
      }
    }
    binarySearchRec(array, elem, 0, array.length)
  }

  def linearSearch(array: Array[Int], elem: Int): Boolean = {
    if( array.headOption==None ) false
    else {
      if (array.head == elem) true
      else linearSearch(array.tail, elem)
    }
  }
/* Another way (using iterator and while)
*
*
  def linear(array: Array[Int], elem: Int): Boolean = {
    var foundOrNot :Boolean = false
    val treatable = array
    val check = treatable.iterator
    while(check.hasNext){
      if (check.next() == elem) foundOrNot = true
    }
    foundOrNot
  }

*
* */

}

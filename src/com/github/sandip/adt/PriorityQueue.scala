package com.github.sandip.adt

// MAX_Heap

class PriorityQueue(capacity: Int = Int.MaxValue){
  val array = new Array[Int](capacity)
  var  count = -1

  def insert(data: Int) = {
    if(count < capacity) {
      count +=1
      array(count)= data
      heapify
    } else  {
      println("Queue is fulled")
    }
  }

  def deleteMax: Int = {
    var data = 0
    if(count >= 0) {
       data = array(0)
      array(0) = array(count)
      array(count) = Int.MinValue
      count -=1
      prolocateDown(0)
    } else  {
      print("\nEmpty Heap !")
    }
    data
  }

  private def prolocateDown(index: Int): Unit = {
    var parentIndex = index
    var left =  parentIndex * 2 + 1
    var right =  parentIndex * 2 + 2
    var maxChild = parentIndex
    if(left <= count && array(left) > array(maxChild) ) {
      maxChild = left
    }
    if(right <= count && array(right) > array(maxChild) ) {
      maxChild = right
    }
    if(maxChild != parentIndex) {
      swap(maxChild, parentIndex)
      prolocateDown(maxChild)
    } else {
      Unit
    }

  }

  private def heapify: Unit = {
    var elemIndex = count
    var parentIndex = (elemIndex-1)/2
    while(array(elemIndex) > array(parentIndex) && elemIndex > 0)   {
      swap(elemIndex, parentIndex)
      elemIndex = parentIndex
      parentIndex = (elemIndex-1)/2
    }
  }

  private def swap(i: Int,j: Int): Unit = {
    val temp = array(i)
    array(i)= array(j)
    array(j)=temp
  }

  def display: Unit = {
    print(s"\nMax heap Elements: ")
    for( i <- 0 to count ) {
      print(s"${array(i)} ")
    }
  }

  def buildHeap(elements : Array[Int]): Unit = {
    elements.foreach {
      case elem if count < capacity =>
        count += 1
        array(count) = elem
    }
    println("After adding elements to heap")
    display
    var parentIndex = 0
    prolocateDown(parentIndex)
    /*
    while(parentIndex < count) {
      parentIndex = (parentIndex -1)/2
      prolocateDown(parentIndex)
    }*/
  }

}

object  Runner {
  def main(args: Array[String]): Unit = {
    val maxHeap = new PriorityQueue(10)
    maxHeap.insert(-10)
    maxHeap.insert(-8)
    maxHeap.insert(-9)
    maxHeap.display
    maxHeap.deleteMax
    maxHeap.display
    maxHeap.deleteMax
    maxHeap.display
    maxHeap.deleteMax
    maxHeap.display
    maxHeap.buildHeap(Array(2,3,4,51,13,42,23,10))
    maxHeap.display
  }
}
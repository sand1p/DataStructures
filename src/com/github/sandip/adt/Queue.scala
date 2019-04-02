package com.github.sandip.adt

class Queue(capacity: Int = Int.MaxValue) {
  private val linkedList = new LinkedList(capacity)

  def enqueue(data: Int) = {
    if(linkedList.size < capacity) {
     linkedList.addAtEnd(data)
    } else {
      println("Queue already fulled. ;)")
    }
  }

  def dequeue = {
    linkedList.head.map { node =>
      println(s"Dequeued: ${node.data}  ;) ")
      linkedList.deleteFirst
      node.data
    }.getOrElse {
      println("No more Elements to retrieve ;)")
      -1
    }
  }

  def display() = {
    linkedList.display
  }

}


object Queue{
  def main(args: Array[String]): Unit = {
    val queue = new Queue(10)
    queue.enqueue(10)
    queue.enqueue(20)
    queue.enqueue(30)
    queue.display
  }
}
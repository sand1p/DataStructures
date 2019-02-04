package com.github.sandip.adt

case class LNode(var data: Int, var  next: Option[LNode] = None)

class LinkedList(capacity: Int = Int.MaxValue) {
  var head: Option[LNode] = None
  var tail: Option[LNode] = None

  private def createNode(data: Int): Option[LNode] = {
    Option(LNode(data=data))
  }

  def get(data: Int): Option[LNode] = {
    var current = head
    var dataNode: Option[LNode] = None
    while(current.nonEmpty) {
      current.foreach{ node =>
          if(node.data == data) {
            current = None
            dataNode = Option(node)
          } else{
            current = node.next
          }
      }
    }
    dataNode
  }

  def addAtBeginning(data: Int): Unit = {
    if(size < capacity) {
      val node = createNode(data)
      node.foreach { n =>
        n.next = head
        head match {
          case Some(h) =>
            if (h.next.isEmpty) {
              tail = head
            }
          case None =>
            tail = node
        }
      }
      head = node
    } else  {
      println("Linked List cannot accomodate more data")
    }
  }

  def addAtEnd(data: Int): Unit = {
    if(size < capacity) {
      val node = createNode(data)
      var current = tail
      current match {
        case Some(n) =>
          n.next = node
        case None =>
          head = node
      }
      tail = node
    } else  {
      println("Linked List cannot accomodate more data")
    }
  }

  def deleteFirst: Unit = {
    head.foreach{ node =>
     head = node.next
    }
  }

  def delete(data: Int): Unit = {
    var current = head
    var prev: Option[LNode] = None
    while(current.nonEmpty) {
      current.foreach { node =>
        if (node.data == data) {
          if (prev.isEmpty) {
            head = node.next
          } else {
            prev.foreach { p =>
              p.next = node.next
            }
          }
          current = None
        } else {
          prev = current
          current = node.next
        }
      }
    }
  }

  def display: Unit = {
    var current = head
    while(current.isDefined) {
      current.foreach { node =>
        println(node.data)
        current = node.next
      }
    }
  }

  def isPresent(data: Int): Boolean = {
    get(data).nonEmpty
  }

  def addAtIndex(index: Int, data: Int) = {
    if(size < capacity) {
      var current = head
      var i = 0
      if(size + 1 < index) {
        Unit
      }
      else  {
        while( i < index ) {
          current.foreach { node =>
            node.next
          }
          i+=1
        }
      }
    } else {
      println("Linked List cannot accomodate more data")
    }
  }

//   TODO
  def deleteAtIndex(index: Int): Unit = {

  }

  def size: Int = {
    var current = head
    var counter = 0
    while(current.isDefined) {
      current.foreach { node =>
        counter +=1
        current = node.next
      }
    }
    counter
  }

  def reverse(): Unit = {
    var current = head
    var prev: Option[LNode] = None
    var next: Option[LNode] = None
    while(current.isDefined) {
       current.foreach { n =>
         next = n.next
         n.next = prev
         prev = current
         current = next
      }
    }
    head = prev
    println(s"New Head: $head")
  }

//  TODO
  def merge(list1: LinkedList, list2: LinkedList ): Option[LNode] = {
     list1.head
  }

}

object LinkedListDriver {
  def main(args: Array[String]): Unit = {
    val linkedList = new LinkedList
    linkedList.addAtBeginning(3)
    linkedList.display
    linkedList.addAtBeginning(5)
    linkedList.display
    linkedList.addAtBeginning(4)
    linkedList.display
    println(s"Get 5: ${linkedList.get(5)}")
    println(s"Get 1: ${linkedList.get(1)}")
    linkedList.display
    println(s"delete 5: ${linkedList.delete(5)}")
    println(s"head: ${linkedList.head}, tail: ${linkedList.tail}")
    linkedList.display
    linkedList.addAtBeginning(5)
    println(s"IsPresent 5: ${linkedList.isPresent(5)}")
    println(s"IsPresent 10: ${linkedList.isPresent(10)}")
    println((s" size: ${linkedList.size}"))
    linkedList.display
    linkedList.reverse()
    linkedList.display
  }
}
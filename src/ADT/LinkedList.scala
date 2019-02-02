package ADT

case class Node( var data: Int, var  next: Option[Node])

class LinkedList {
  var head: Option[Node] = None
  var tail: Option[Node] = None

  private def createNode(data: Int): Option[Node] = {
    Option(Node(data, None))
  }

  def get(data: Int): Option[Node] = {
    var current = head
    var dataNode: Option[Node] = None
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
    val node = createNode(data)
    node.foreach{ n =>
      n.next = head
      head match {
      case  Some(h) =>
        if(h.next.isEmpty) {
          tail = head
        }
      case None =>
        tail = node
      }
    }
    head = node
  }

  def addAtEnd(data: Int): Unit = {
    val node = createNode(data)
    var current = tail
    current match {
      case Some(n) =>
        n.next = node
      case None =>
        head = node
    }
    tail = node
  }

  def deleteFirst: Unit = {
    head.foreach{ node =>
     head = node.next
    }
  }

  def delete(data: Int): Unit = {
    var current = head
    var prev: Option[Node] = None
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
  }

  def deleteAtIndex(index: Int) = {

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

  def reverse() = {
    var current = head
    var prev: Option[Node] = None
    var next: Option[Node] = None
    while(current.isDefined) {
       current.foreach{ n =>
         next = n.next
         n.next = prev
         prev = current
         current = next
      }
    }
    head = prev
    println(s"New Head: $head")
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
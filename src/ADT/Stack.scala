package ADT

class Stack(capacity: Int = Int.MaxValue) {
  private val linkedList = new LinkedList(capacity)

  def pop: Int = {
    linkedList.head.map { node =>
      println(s"Popped: ${node.data}")
      linkedList.deleteFirst
      node.data
    }.getOrElse {
      println("Underflow")
      -1
    }
  }

  def push(data: Int) = {
    if (linkedList.size < capacity) {
      val oldSize = linkedList.size
      linkedList.addAtBeginning(data)
    } else {
      println("Stack Overflow..!!")
    }
  }

  def stackTop: Int = {
    linkedList.head.map{ node =>
      node.data
    }.getOrElse {
      println("Underflow")
      -1
    }
  }

  def display = {
    linkedList.display
  }
}

object Stack {
  def main(args: Array[String]): Unit = {
    val CAPACITY = 5
    val stack = new Stack(CAPACITY)
    stack.push(4)
    stack.push(2)
    stack.push(7)
    stack.push(70)
    stack.display
  }
}
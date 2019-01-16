
class MyQueue(SIZE: Int= 5) {
  private var queue: Array[Int] = new Array[Int](SIZE)
  private var front: Int = 0
  private var rare: Int = 0

  def enqueue(value: Int) = {
    isFull match {
      case true =>
        println("Overflow")
      case false =>
        queue(rare) = value
        rare += 1
        rare = rare % SIZE
    }
  }

  def dequeue: Int = {
    isEmpty match {
      case true =>
        println("underflow")
        -1
      case false =>
        val value = queue(front)
        front += 1
        value
    }
  }

  def isEmpty: Boolean = rare == front

  def nonEmpty: Boolean = !isEmpty

  def isFull: Boolean = (rare - front == SIZE - 1 && rare > front) || (rare == front - 1)

  val size = if(rare >= front) rare - front else SIZE - front + rare


  override def toString: String = {
    s"front: $front rare: $rare "
  }

}




object MyStack {
  val SIZE: Int = 2
  val queue1 = new MyQueue(2)
  val queue2 = new MyQueue(2)

  def pop: Int = {
    if (queue1.nonEmpty) {
      while (queue1.size > 1) {
        queue2.enqueue(queue1.dequeue)
      }
      val value = queue1.dequeue
      while (queue2.nonEmpty)
        queue1.enqueue(queue2.dequeue)
      value
    } else {
      println("underflow")
      -1
    }
  }

  def push(element: Int) = {
    if (queue1.isFull) {
      println("overflow")
    } else {
      queue1.enqueue(element)
    }
  }

  def top: Int = {
    if (queue1.nonEmpty) {
      while (queue1.size > 1) {
        queue2.enqueue(queue1.dequeue)
      }
      val value = queue1.dequeue
      queue2.enqueue(value)
      while (queue2.nonEmpty)
        queue1.enqueue(queue2.dequeue)
      value
    } else {
      println("underflow")
      -1
    }
  }

}
MyStack.push(2)
println(MyStack.queue1.toString)
MyStack.pop
MyStack.pop




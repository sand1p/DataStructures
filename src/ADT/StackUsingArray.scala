package ADT

class StackUsingArray(capacity: Int= Int.MaxValue) {
  private val array = Array[Int](capacity)
  private var top: Int = -1
  private val size: Int = array.size

  def pop: Int = {
    if(top < 0) {
      println("Underflow...!")
      -1
    } else {
      val data = array(top)
      top -=1
      data
    }
  }

  def push(data: Int) = {
    if(top < capacity) {
      top +=1
      array(top) = data
    } else {
      println("Overflow...!")
    }
  }

  def stackTop : Int = {
   if(top < 0) {
     println("Underflow")
     -1
   } else {
     array(top)
   }
  }
}


object  StackUsingArray {
  def main(args: Array[String]): Unit = {
    val CAPACITY = 10
    val stack= new StackUsingArray(CAPACITY)

  }
}

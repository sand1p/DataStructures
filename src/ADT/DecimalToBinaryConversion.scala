package ADT

object DecimalToBinaryConversion {
  private val stack = new Stack
//  val binaryString : String = ""
  def decimalToBindary(decimalNumber : Int): Unit = {
    decimalNumber > 0 match {
      case true if decimalNumber > 1 =>
        stack.push(decimalNumber % 2)
        decimalToBindary(decimalNumber / 2)
      case true if decimalNumber == 1 =>
        stack.push(decimalNumber)
        decimalToBindary(0)
      case false =>
        showBinaryNumber
    }
  }
  def showBinaryNumber: Unit = {
    while(stack.nonEmpty) {
      stack.pop
    }
  }

  def main(args: Array[String]): Unit = {
    decimalToBindary(100)
  }
}

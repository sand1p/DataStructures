package com.github.sandip.applications.stack

object ReverseStack {

  def reverse[T](stack: Stack[T]): Stack[T]= {
    val aux = new Stack[T]
    while(stack.nonEmpty) {
      aux.push(stack.pop)
    }
    aux
  }

  def main(args: Array[String]): Unit = {
    var stack = new Stack[Int]
    stack.push(1)
    stack.push(2)
    stack.push(4)
    stack.push(6)
    print("before reversing: ")
    while(stack.nonEmpty) {
      print(s"${stack.pop} ")
    }
    stack.push(1)
    stack.push(2)
    stack.push(4)
    stack.push(6)
    stack = reverse(stack)


    print("  After reversing: ")
    while(stack.nonEmpty) {
      print(s"${stack.pop} ")
    }
  }
}

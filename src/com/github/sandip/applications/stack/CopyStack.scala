package com.github.sandip.applications.stack


/**
  * Write an algorithm that copies content of one stack to another
  */
object CopyStack {

  val sourceStack = new Stack[Int]
  val destinationStack = new Stack[Int]

  def initialize() = {
    for (i <- 1 to 10)
      yield  sourceStack.push(i)
   }

  def copyStack(sourceStack: Stack[Int]): Unit = {
      if(sourceStack.nonEmpty){
        val data = sourceStack.pop
        copyStack(sourceStack)
        destinationStack.push(data)
      }
  }

  def main(args: Array[String]): Unit = {
    initialize()
    copyStack(sourceStack)
    initialize()
    println("Source Stack ")
    while (sourceStack.nonEmpty) {
      print(s" ${sourceStack.pop}")
    }

    println("\n Destination Stack: ")
    while (destinationStack.nonEmpty) {
      print(s" ${destinationStack.pop}")
    }
  }
}

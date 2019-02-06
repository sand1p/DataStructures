package com.github.sandip.applications.stack


/**
  * concatenate content of one stack on top of another
  * Use stack ADT only.
  */
object ConcatStack {

  def initialize[T](stack: Stack[T]): Stack[T] = {
    for (i <- 1 to 5)
      yield
        stack.push(i.asInstanceOf[T])
    stack
  }

  def catStack[T](source: Stack[T], dest: Stack[T]):Unit = {
    if(source.nonEmpty) {
      val data = source.pop
      catStack(source,dest)
      dest.push(data)
    }
  }

  def main(args: Array[String]): Unit = {
    var source = new Stack[String]
    var destination = new Stack[String]

    source =initialize(source)
    destination = initialize(destination)

    catStack(source, destination)

    while(destination.nonEmpty) {
      print(s" ${destination.pop}")
    }
  }

}

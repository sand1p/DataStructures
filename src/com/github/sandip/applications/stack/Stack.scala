package com.github.sandip.applications.stack

class Stack[T]{
  private var elements: List[T] = Nil
  def pop: T = {
    val currentTop = peek
    elements = elements.tail
    currentTop
  }
  def push(data: T): Unit =   elements = data :: elements
  def peek: T = elements.head
}

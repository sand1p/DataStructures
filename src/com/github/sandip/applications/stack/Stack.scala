package com.github.sandip.applications.stack

class Stack[T] {
  private var elements: List[T] = Nil

  def pop: T = {
    if(nonEmpty) {
      val currentTop = peek
      elements = elements.tail
      currentTop
    } else {
      0.asInstanceOf[T]
    }
  }

  def peek: T = {
    if (nonEmpty) {
      elements.head
    } else {
      (-1).asInstanceOf[T]
    }
  }

  def push(data: T): Unit = elements = data :: elements

  def nonEmpty: Boolean = !isEmpty

  def isEmpty: Boolean = elements.isEmpty

}
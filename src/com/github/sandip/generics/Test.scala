package com.github.sandip.generics

class Test[T](obj: T) {
  def display = {
    println(s"element received: $obj")
  }
}

object Main {
  def main(args: Array[String]): Unit = {
    val newObject = new Test[String]("Sandip")
    newObject.display

    val intObject = new Test[Int](10)
    intObject.display


    val doubleObject = new Test[Double](10.2421341)
    doubleObject.display
  }

}

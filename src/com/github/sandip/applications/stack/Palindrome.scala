package com.github.sandip.applications.stack

object Palindrome {
  def isPalindrome(str: String): Boolean = {
    val stack = new Stack[Char]
    str.foreach { char =>
      stack.push(char)
    }
    str.forall(char => stack.pop.equals(char))
  }

  def main(args: Array[String]): Unit = {
    val inputString = "ababa"
    println(isPalindrome(inputString))
  }

}
package com.github.sandip.applications.stack

import scala.languageFeature.postfixOps

object ExpressionConverter {
  private val operators: Map[Char, Int] =
    Map(
    '('  -> 5,
    '1'  -> 0,
    ')'  -> 5,
    '+'  -> 1,
    '-'  -> 1,
    '*'  -> 2,
    '\\' -> 2)

  private val characters = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"

  def infixToPostFix(infixString: String) : String = {
    val stack = new Stack[Char]
    var postfixString: String = ""
    infixString.foreach { newChar =>
      if (characters.contains(newChar)) {
        postfixString += newChar
      } else {
        if (stack.isEmpty) {
          stack.push(newChar)
        } else {
          operators.get(newChar).foreach { newCharPriority =>
            operators.get(stack.peek).foreach { stackTop =>
              if (newCharPriority <= stackTop) {
                postfixString += iterateStack(stack)
              }
              stack.push(newChar)
            }
          }
        }
      }
    }
    postfixString += iterateStack(stack)
    postfixString
  }

  def iterateStack(stack: Stack[Char]) = {
    var postfix = ""
    while (stack.nonEmpty) {
      postfix += stack.pop
    }
    postfix
  }

  def main(args: Array[String]): Unit = {
    val INFIX: String = "a*b+c*d"
    val postFix: String = infixToPostFix(INFIX)
    println(s" Infix: $INFIX \n postfix: $postFix")
  }

}

package com.github.sandip.adt

// Binary search tree
import com.github.sandip.applications.stack.Stack

import scala.collection.mutable

case class TreeNode(var left: Option[TreeNode] = None, var right: Option[TreeNode] = None, data: Int)

class Tree {
  private var root: Option[TreeNode] = None

  def inorder = {
    val stack = new Stack[TreeNode]
    var current = root
    while (current.nonEmpty || stack.nonEmpty) {
      while (current.nonEmpty) {
        current.foreach { node =>
          stack.push(node)
          current = node.left
        }
      }
      if (stack.nonEmpty) {
        val top = stack.pop
        print(s" ${top.data}")
        current = top.right
      }
    }
  }

  def add(data: Int): Unit = {
    var current = root
    current match {
      case None =>
        root = Option(TreeNode(data = data))
      case Some(cur) =>
        while (current.nonEmpty) {
          current.foreach { node =>
            if (data < node.data) {
              current = node.left.orElse {
                node.left = Option(TreeNode(data = data))
                None
              }
            } else {
              current = node.right.orElse {
                node.right = Option(TreeNode(data = data))
                None
              }
            }
          }
        }
    }
  }

  def postorder = {
    val stackl = new Stack[TreeNode]()
    val current = root
    val stack2 = new Stack[TreeNode]()
    current.foreach(node => stackl.push(node))
    while (stackl.nonEmpty) {
      val node = stackl.pop
      stack2.push(node)
      node.left.foreach(left => stackl.push(left))
      node.right.foreach(right => stackl.push(right))
    }
    while (stack2.nonEmpty) {
      print(s" ${stack2.pop.data}")
    }
  }

  def preorder = {
    val stack = new Stack[TreeNode]
    var current = root
    while (current.nonEmpty) {
      current.foreach { node =>
        print(s" ${node.data}")
        node.right.foreach { right =>
          stack.push(right)
        }
        current = node.left.orElse {
          if (stack.nonEmpty) {
            Option(stack.pop)
          } else {
            None
          }
        }
      }
    }
  }

  def countNodes: Int = {
    def counter(current: Option[TreeNode]): Int = {
      current match {
        case Some(node) => 1 + counter(node.left) + counter(node.right)
        case None => 0
      }
    }
    counter(root)
  }

  def countNodesWithSingleOrNoChild: Int = {
    def counter(current: Option[TreeNode]): Int = {
      current match {
        case Some(node) => 0 + counter(node.left) + counter(node.right)
        case None => 1
      }
    }

    counter(root) / 2
  }

  //write an algorithm that counts the number of leaves in a binary tree
  def countLeaves: Int = {
    def counter(current: Option[TreeNode]): Int = {
      current match {
        case Some(node) if node.left.isEmpty && node.right.isEmpty => 1
        case Some(node) => counter(node.left) + counter(node.right)
        case None => 0
      }
    }

    // write an algorithm that tells whether tree is complete
    counter(root)
  }

  def bfs = {
    val queue = new mutable.Queue[TreeNode]
    var current = root
    while (current.nonEmpty) {
      current.foreach { node =>
        print(s" ${node.data}")
        node.left.foreach { left =>
          queue.enqueue(left)
        }
        node.right.foreach { right =>
          queue.enqueue(right)
        }
        if (queue.nonEmpty) {
          current = Option(queue.dequeue)
        } else {
          current = None
        }
      }
    }
  }

  private def createNode(data: Int): TreeNode = {
    //named argument
    TreeNode(data = data)
  }


}


object TreeDriver {
  def main(args: Array[String]): Unit = {
    val tree = new Tree
    tree.add(10)
    println("added 10")
    tree.add(20)
    println("added 20")
    tree.add(5)
    println("added 5")
    tree.add(25)
    println("added 25")
    tree.add(15)
    println("added 15")
    tree.add(30)
    println("added 30")
    tree.add(7)
    println("added 7")
    println("Preorder traversal: ")
    tree.preorder
    println(" \nInorder traversal: ")
    tree.inorder
    println(" \n Postorder traversal: ")
    tree.postorder
    println(" \n Breadth First traversal: ")
    tree.bfs
    println(s" \n number of nodes: ${tree.countNodes}")
    tree.bfs
    println(s" \n number of nodes with 1 or no childs ${tree.countNodesWithSingleOrNoChild}")

    println(s" \n number of leaves: ${tree.countLeaves}")

  }
}
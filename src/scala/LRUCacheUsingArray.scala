package scala

import scala.collection.mutable.{ArrayBuffer => Cache}

case class Node(key: Int, value: String, var timestamp: Long)

object LRUCacheUsingArray {
  def main(args: Array[String]): Unit = {
    initialize()
    println(cache)
    println(get(4))
    set(Node(11, "value11", getTimestamp))
    println(cache)
    println(get(11))
  }

  private val initialSize  = 10
  private val cache = new Cache[Node](initialSize)

  private def initialize(): Unit = {
    val range = 1 to 10
    range foreach{ i =>
      cache+= Node( i, "value"+i, getTimestamp)
    }
  }

  def get(key : Int): String = {
    cache.find( _.key == key) match {
      case None => "Element not found"
      case Some(node) =>
        access(node)
        node.value
    }
  }

  def access(elem: Node) = {
    cache.filter(_ equals elem)
      .map(delete(_))
    elem.timestamp=getTimestamp
    cache+=elem
  }

  private def getTimestamp: Long = {
    System.nanoTime()
  }

  def set( element : Node ) = {
    println(s"number of elements  size : ${cache.size} ")
    println(s"number of elements  length : ${cache.length} ")

    if(cache.size == initialSize)  {
      println(" CAche is fulled replacing LRU element from cache.")
      replace(element)
    } else {
      cache += element
    }
  }

  private def replace(element: Node) = {
    var min = getTimestamp
    cache.foreach { elem =>
      min = if(elem.timestamp < min) elem.timestamp else min
    }
    delete(element, Some(min))
  }

  private def delete(elem: Node, timestamp: Option[Long] = None) = {
    timestamp match {
      case  Some(time) =>
        cache.filter(_.timestamp == time).foreach{ lruElem =>
          println(s"LRU element to be replaced is $lruElem")
          cache-=lruElem
        }
        cache+=elem
      case None => cache-=elem
    }
  }


}
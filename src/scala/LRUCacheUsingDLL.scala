import scala.collection.mutable.{OpenHashMap => Cache}

case class Student(rollNo: Int, name: String)

class DNode(
             var prev: Option[DNode],
             var student: Student,
             var next: Option[DNode]
           ) {

  override def toString() = {
    s"${student.name}"
  }
}

object LRUCache {
  val CAPACITY = 3
  val cache = new Cache[Int, DNode](CAPACITY)
  var head: Option[DNode] = None
  var tail: Option[DNode] = None

  def set(student: Student) = {
    val key = student.rollNo
    cache.get(key) match {
      case None =>
        if (cache.size < CAPACITY) {
          head match {
            case None =>
              head = Some(new DNode(None, student, None))
              tail = head
              cache.put(key, head.get)
            case Some(node) =>
              addAtEnd(new DNode(None, student, None))
              cache.put(key, tail.get)
          }
        } else {
          replacement(student)
        }
      case Some(_) =>
        println("Record already exists, cacnnot be override")
    }
  }

  private def replacement(student: Student) = {
    val lru: Option[DNode] = getLRU
    lru match {
      case Some(node) =>
        cache.remove(node.student.rollNo)
        delete(node)
        val newNode = new DNode(None, student, None)
        addAtEnd(newNode)
        cache.put(student.rollNo,newNode)
      case None => println("head not found")
    }
  }

  private def getLRU: Option[DNode] = {
    head
  }

  def get(key: Int): String = {
    if (cache.isEmpty) {
      "-1"
    } else {
      val data = cache.get(key)
      data match {
        case None => "-1"
        case Some(node) =>
          cache.remove(key)
          delete(node) match {
            case true =>
              addAtEnd(new DNode(None, node.student, None))
              cache.put(key,tail.get
              )
            case _ =>
          }
          node.student.name
      }
    }
  }

  def addAtEnd(node : DNode) = {
    node.prev= tail
    tail.get.next = Some(node)
    tail = Some(node)
  }

  private def delete(node: DNode) = {
    val tailKey = tail.map(_.student.rollNo).getOrElse(0)
    val headKey = head.map(_.student.rollNo).getOrElse(0)
    val nodeKey = node.student.rollNo
    if (nodeKey == tailKey) {
      false
    } else if (nodeKey == headKey) {
      deleteHead(node)
      true
    } else {
      node.prev.get.next = node.next
      node.next.get.prev = node.prev
      true
    }
  }

  def deleteHead(node: DNode) = {
    node.next.get.prev = None
    head = head.get.next
  }
}

LRUCache.set(Student(2, "s"))
println(LRUCache.cache)

LRUCache.set(Student(3, "p"))
println(LRUCache.cache)

println(LRUCache.get(2))
println(LRUCache.cache)


LRUCache.set(Student(4, "d"))
println(LRUCache.cache)


println(LRUCache.get(2))
println(LRUCache.cache)


LRUCache.set(Student(5, "f"))
println(LRUCache.cache)

LRUCache.set(Student(6, "g"))
println(LRUCache.cache)

LRUCache.set(Student(7, "h"))
println(LRUCache.cache)
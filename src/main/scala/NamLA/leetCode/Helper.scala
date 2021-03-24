package NamLA.leetCode

import scala.collection.mutable

object Helper {

    class TreeNode(var value: Int, var left: TreeNode = null, var right: TreeNode = null)

    class ListNode(var x: Int = 0, var next: ListNode = null) {
        def printData(): Unit = {
            var data = mutable.ArrayBuffer[Int]()
            data.append(x)
            var n = next
            while (n != null) {
                data.append(n.x)
                n = n.next
            }
            println(data.mkString(","))
        }
    }

    object ListNode {
        def apply(a : Array[Int]): ListNode = {
            val head = new ListNode()
            var node = head
            a.foreach(i => {
                node.next = new ListNode(i)
                node = node.next
            })
            head.next
        }

        def convertArray(a: Array[Array[Int]]): Array[ListNode] = {
            a.map(d => ListNode(d))
        }
    }

    def getMinHeap(): mutable.PriorityQueue[Int] = collection.mutable.PriorityQueue.empty[Int]((x: Int, y: Int) => y.compare(x))

    def toArrArr(data: String): Array[Array[Int]] = {
        data.drop(1).dropRight(1).split("\\],\\[").map(i => {
            i.replaceAll("\\[", "").replaceAll("\\]", "").
                split(",").filterNot(_.isEmpty).
                map(j => j.toInt)
        })
    }
    def toArr(data: String): Array[Int] = {
        data.drop(1).dropRight(1).split(",").map(i => i.trim.toInt)
    }
    def to2Arr(data: String): (Array[Int], Array[Int]) = {
        val x = data.split("\n").map(i => toArr(i))
        (x(0), x(1))
    }
    def toArrString(data: String): Array[String] = {
        data.drop(1).dropRight(1).split(",")
    }
}

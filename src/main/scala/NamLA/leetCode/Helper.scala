package NamLA.leetCode

import scala.collection.mutable

object Helper {

    class TreeNode(var value: Int, var left: TreeNode = null, var right: TreeNode = null)

    class ListNode(var x: Int = 0, var next: ListNode = null) {

        def toArr: Array[Int] = {
            val data = mutable.ArrayBuffer[Int]()
            data.append(x)
            var n = next
            while (n != null) {
                data.append(n.x)
                n = n.next
            }
            data.toArray
        }
        def printData(): Unit = {
            println(toArr.mkString(","))
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

        def revere(node: ListNode): ListNode = {
            if (node == null || node.next == null)
                return node
            var next = node.next
            node.next = null
            var tail = node
            while (next != null) {
                val tmp = next.next
                next.next = tail
                tail = next
                next = tmp
            }
            tail

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

    def toArrAndInt(data: String): (Array[Int], Int) = {
        val Array(dataArr: String, dataInt: String) = data.split("\n")
        val arr = toArr(dataArr)
        (arr, dataInt.toInt)
    }

    def toArrArrAndInt(data: String): (Array[Array[Int]], Int) = {
        val Array(dataArr: String, dataInt: String) = data.split("\n")
        val arr = toArrArr(dataArr)
        (arr, dataInt.toInt)
    }
}

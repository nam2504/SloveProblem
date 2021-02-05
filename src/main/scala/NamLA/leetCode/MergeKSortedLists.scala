package NamLA.leetCode

import NamLA.leetCode.Helper._

object MergeKSortedLists {
    def mergeKLists(lists: Array[ListNode]): ListNode = {
        var rs = lists
        while (rs.length > 1) {
            rs = rs.grouped(2).map(i =>
                merge(i)
            ).toArray
        }
        rs.head
    }

    def merge(data: Array[ListNode]) : ListNode = {
        if (data.length == 1)
            return data.head
        merge(data(0), data(1))
    }

    def merge(a: ListNode, b: ListNode): ListNode = {
        (a, b) match {
            case (null, null) => null
            case (a, null) => a
            case (null, b) => b
            case (a, b) if a.x < b.x =>
                a.next = merge(a.next, b)
                a
            case (a, b) if a.x >= b.x =>
                b.next = merge(b.next, a)
                b
            case _ => null
        }
    }

    def main(args: Array[String]): Unit = {
        test("[[1,4,5],[1,3,4],[2,6]]")
    }

    def test(data: String) = {
        val arr = toArrArr(data)
        val x = arr.map(d => ListNode(d))
        val rs = mergeKLists(x)
        if (rs != null)
            rs.printData()
    }
}

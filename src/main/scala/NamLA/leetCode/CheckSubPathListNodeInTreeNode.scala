package NamLA.leetCode

import NamLA.leetCode.Helper._

object CheckSubPathListNodeInTreeNode {

    def isSubPath(head: ListNode, root: TreeNode): Boolean = {
        def findHead(node: TreeNode = root): Boolean = {
            if (node == null)
                return false
            if (node.value == head.x) {
                if (checkSubPath(head, node)) {
                    return true
                }
            }
            findHead(node.left) || findHead(node.right)
        }
        findHead()
    }
    def checkSubPath(h: ListNode, node: TreeNode): Boolean = {
        if (h == null)
            return true
        if (node == null)
            return false

        if (h.x != node.value)
            return false
        checkSubPath(h.next, node.left) || checkSubPath(h.next, node.right)
    }
    val Max: Int = (1e9 +7).toInt
    println(Max)

    def main(args: Array[String]): Unit = {
        val nums = collection.mutable.ArrayBuffer[Int]()
        nums.clear()
        nums.append(2)
        nums.append(3)
        println(nums.takeRight(1).product)
    }
}

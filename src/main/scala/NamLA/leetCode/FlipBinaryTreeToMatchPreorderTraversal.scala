package NamLA.leetCode

import NamLA.leetCode.Helper._

object FlipBinaryTreeToMatchPreorderTraversal {

    def flipMatchVoyage(root: TreeNode, voyage: Array[Int]): List[Int] = {
        val flip = collection.mutable.ArrayBuffer[Int]()
        val queue = collection.mutable.Queue(root)
        var i = 0

        def preOrder(n: TreeNode): Boolean = {
            if (n.right != null && n.right.value == voyage(i + 1)) {
                // filp at node
                val tmp = n.left
                n.left = n.right
                n.right = tmp
                flip.append(n.value)
            }

            if (n.left != null) {
                //skip to left
                i += 1
                if (n.left.value != voyage(i)) {
                    return false
                }
                preOrder(n.left)
            }
            if (n.right != null)
                queue.enqueue(n.right)
            true
        }

        while (queue.nonEmpty) {
            val n = queue.dequeue()
            if (n.value != voyage(i)) {
                return List(-1)
            }
            if (!preOrder(n)) {
                return List(-1)
            }
            i += 1
        }


        flip.toList
    }

    def main(args: Array[String]): Unit = {

    }
}

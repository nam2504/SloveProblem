package NamLA.leetCode

import NamLA.leetCode.Helper.TreeNode

class BtreeGameWinningMove {
    def btreeGameWinningMove(root: TreeNode, n: Int, x: Int): Boolean = {
        var leftX, rightX = 0

        def countNode(node: TreeNode): Int = {
            if (node == null)
                return 0
            val left = countNode(node.left)
            val right = countNode(node.right)
            if (node.value == x) {
                leftX = left
                rightX = right
            }
            left + right + 1
        }

        val total = countNode(root)
        val max = leftX.max(rightX).max(total - leftX - rightX - 1)
        max > n / 2
    }


}

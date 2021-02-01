package NamLA.leetCode
import Helper._

object VerticalTraversal {

    def verticalTraversal(root: TreeNode): List[List[Int]] = {
        val map: collection.mutable.Map[Int, collection.mutable.SortedSet[(Int, Int)]] = collection.mutable.Map()
        def travel(node: TreeNode = root, x: Int = 0, y: Int = 0): Unit = {
            if (node == null) {
                return
            }
            // add data :
            val all = map.getOrElseUpdate(x, collection.mutable.SortedSet[(Int, Int)]())
            all.add((node.value, y))
            travel(node.left, x - 1, y - 1)
            travel(node.right, x + 1, y - 1)
        }
        travel()
        val data = map.toList.sortBy(_._1).map(_._2)
        data.map(i => i.toList.map(_._1))
    }
}

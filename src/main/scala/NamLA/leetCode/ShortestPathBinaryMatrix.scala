package NamLA.leetCode

object ShortestPathBinaryMatrix {
    def shortestPathBinaryMatrix(grid: Array[Array[Int]]): Int = {
        val n = grid.length - 1
        val m = grid.head.length - 1
        if (grid(0)(0) == 1 || grid(n)(m) == 1) return -1
        if (n == 0 && m == 0) return 1
        var node = Array((0, 0))
        grid(0)(0) = 1
        while (node.nonEmpty) {
            val ab = collection.mutable.ArrayBuffer[(Int, Int)]()
            for ((x, y) <- node) {
                for (i <- (x - 1).max(0) to (x + 1).min(n))
                    for (j <- (y - 1).max(0) to (y + 1).min(m)) {
                        if (grid(i)(j) == 0) {
                            grid(i)(j) = grid(x)(y) + 1
                            if ( i == n && j == m)
                                return grid(i)(j)
                            ab.append((i, j))
                        }
                    }
            }
            node = ab.toArray
        }
        -1
    }

    def test(data: String): Unit = {
        println(shortestPathBinaryMatrix(Helper.toArrArr(data)))
    }

    def main(args: Array[String]): Unit = {
        test("[[0,1],[1,0]]") //2
        test("[[0,0,0],[1,1,0],[1,1,0]]") // 4
    }
}

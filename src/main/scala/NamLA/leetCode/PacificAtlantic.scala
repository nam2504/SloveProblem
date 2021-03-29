package NamLA.leetCode

object PacificAtlantic {

    def pacificAtlantic(matrix: Array[Array[Int]]): List[List[Int]] = {
        def moveFrom(isP : Boolean = true): Array[Array[Boolean]] = {
            if (matrix.length <= 1 || matrix.headOption.getOrElse(Array.empty).length <= 1) {
                return Array.fill(matrix.length, matrix.headOption.getOrElse(Array.empty).length)(true)
            }

            val canMove = Array.fill(matrix.length, matrix.head.length)(false)
            val queue = collection.mutable.Queue[(Int, Int)]()

            for (i <- matrix.indices) {
                if (isP)
                    queue.append((i, 0))
                else
                    queue.append((i, matrix.head.length - 1))
            }

            for (j <- matrix.head.indices) {
                if (isP)
                    queue.append((0, j))
                else
                    queue.append((matrix.length - 1, j))
            }


            while (queue.nonEmpty) {
                val (i, j) = queue.dequeue()
                if (!canMove(i)(j)) {
                    canMove(i)(j) = true
                    // try to move neighbor
                    if (i > 0 && !canMove(i - 1)(j) && matrix(i - 1)(j) >= matrix(i)(j))
                        queue.append((i - 1, j))
                    if (i < matrix.length - 1 && !canMove(i + 1)(j) && matrix(i + 1)(j) >= matrix(i)(j))
                        queue.append((i + 1, j))

                    if (j > 0 && !canMove(i)(j - 1) && matrix(i)(j - 1) >= matrix(i)(j))
                        queue.append((i, j - 1))
                    if (j < matrix.head.length - 1 && !canMove(i)(j + 1) && matrix(i)(j + 1) >= matrix(i)(j))
                        queue.append((i, j + 1))
                }
            }
            canMove
        }

        val p = moveFrom()
        val a = moveFrom(false)

        val lb = collection.mutable.ListBuffer[List[Int]]()
        for (i <- matrix.indices) {
            for (j <- matrix.head.indices)
                if (p(i)(j) && a(i)(j))
                    lb.append(List(i, j))
        }
        lb.toList
    }



    def main(args: Array[String]): Unit = {

    }
}

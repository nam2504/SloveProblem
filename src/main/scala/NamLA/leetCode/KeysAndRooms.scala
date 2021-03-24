package NamLA.leetCode

object KeysAndRooms {
    def canVisitAllRooms(rooms: List[List[Int]]): Boolean = {
        val visited    = Array.fill(rooms.length)(false)

        def visit(i: Int = 0): Unit = {
            if (visited(i))
                return
            visited(i) = true
            val keys = rooms(i)
            for (k <- keys) {
                if (!visited(k)) {
                    visit(k)
                }
            }
        }

        !visited.tail.contains(false)
    }

    def main(args: Array[String]): Unit = {
//        test("[[1],[],[0,3],[1]]") // false

        test("[[1],[2],[3],[]]") //true
//        test("[[1,3],[3,0,1],[2],[0]]") // false
    }

    def test(data: String): Unit = {
        val arr = Helper.toArrArr(data)
        println(canVisitAllRooms(arr.map(i => i.toList).toList))
    }
}

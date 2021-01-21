package NamLA.leetCode

object DeliveringBoxesFromStorageToPorts {
    def boxDelivering(boxes: Array[Array[Int]], portsCount: Int, maxBoxes: Int, maxWeight: Int): Int = {
        // trip from i to i + 1
        def trip(i: Int): Int = {
            if (i < 0 || i == boxes.length - 1 || boxes(i)(0) == boxes(i + 1)(0))
                0
            else
                1
        }

        var w = 0
        var t = 0 // trip from start to i
        var start = 0

        def nextStart(): Unit = {
            w -= boxes(start)(1)
            t -= trip(start)
            start += 1
        }

        val dp = Array.fill(boxes.length + 1)(0)

        for (i <- boxes.indices) {
            if (i - start == maxBoxes) {
                nextStart()
            }
            w += boxes(i)(1)
            t += trip(i - 1)

            while (w > maxWeight || (start < i && dp(start) == dp(start + 1))) {
                nextStart()
            }
            dp(i + 1) = dp(start) + t + 2 // -1 to start (1), start to i (t) ,i to -1 (1)
        }
        dp.last
    }

    def main(args: Array[String]): Unit = {
        test(Array(Array(1, 2), Array(3, 3), Array(3, 1), Array(3, 1), Array(2, 4)), 3, 3, 6) // 6
        test(Array(Array(1, 1), Array(2, 1), Array(1, 1)), 2, 3, 3) // 4
        test(Array(Array(1, 1), Array(2, 1)), 2, 3, 3) // 3
        test(Array(Array(1, 4), Array(1, 2), Array(2, 1), Array(2, 1), Array(3, 2), Array(3, 4)), 3, 6, 7) //6
        test(Array(Array(2, 4), Array(2, 5), Array(3, 1), Array(3, 2), Array(3, 7), Array(3, 1), Array(4, 4), Array(1, 3), Array(5, 2)),5,5,7) //14
    }

    def test(boxes: Array[Array[Int]], portsCount: Int, maxBoxes: Int, maxWeight: Int): Unit = {
        println(boxDelivering(boxes, portsCount, maxBoxes, maxWeight))
    }
}

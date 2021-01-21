package NamLA.leetCode

object UnHappyFriends {
    def unhappyFriends(n: Int, preferences: Array[Array[Int]], pairs: Array[Array[Int]]): Int = {
        val p = Array.fill(n, n)(0)
        val unHappy = Array.fill(n)(0)
        for (i <- preferences.indices) {
            for (j <- preferences(i).indices) {
                p(i)(preferences(i)(j)) = n - j
            }
        }

        def unhappy(a: Array[Int], b: Array[Int]): Unit = {
            if (a == b || a(0) == b(0) || a(0) == b(1) || a(1) == b(0) || a(1) == b(1) )
                return
            if (prefer(a(0), b(0), a(1)) && prefer(b(0), a(0), b(1))) {
                unHappy(a(0)) = 1
                unHappy(b(0)) = 1

            }
            if (prefer(a(0), b(1), a(1)) && prefer(b(1), a(0), b(0))) {
                unHappy(a(0)) = 1
                unHappy(b(1)) = 1
            }
            if (prefer(b(0), a(1), b(1)) && prefer(a(1), b(0), a(0))) {
                unHappy(a(1)) = 1
                unHappy(b(0)) = 1
            }
            if  (prefer(b(1), a(1), b(0)) && prefer(a(1), b(1), a(0))) {
                unHappy(a(1)) = 1
                unHappy(b(1)) = 1
            }
        }

        def prefer(x: Int, u: Int, y: Int): Boolean = {
            p(x)(u) > p(x)(y)
        }

        pairs.foreach(i =>
            pairs.foreach(j =>
                unhappy(i, j)
            )
        )
        unHappy.sum
    }

    def main(args: Array[String]): Unit = {
        println(unhappyFriends(4,
            Array(Array(1, 2, 3), Array(3, 2, 0), Array(3, 1, 0), Array(1, 2, 0)),
            Array(Array(0, 1), Array(2, 3)))
        ) // 2
    }
}

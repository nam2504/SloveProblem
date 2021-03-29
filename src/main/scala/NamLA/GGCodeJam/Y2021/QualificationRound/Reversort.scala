package NamLA.GGCodeJam.Y2021.QualificationRound


object Reversort {

    import GGCJHelper._

    def rever(i: Int, j: Int, data: Array[Int]): Unit = {
        var start = i
        var end = j
        while (start < end) {
            val tmp = data(start)
            data(start) = data(end)
            data(end) = tmp
            start += 1
            end -= 1
        }
    }

    def reverSortCost(data: Array[Int]): Int = {
        var cost = 0
        for (i <- 0 until data.length - 1) {
            var min = i
            for (j <- i + 1 until data.length) {
                if (data(j) < data(min)) {
                    min = j
                }
            }
            rever(i, min, data)
            cost += min - i + 1
        }
        cost
    }

    def reverToCost(n: Int, cost: Int): Array[Int] = {
        if (cost < n - 1 || cost > n*(n + 1) / 2) return  Array.empty
        val data = Range(1, n + 1).toArray
        if (cost == n - 1)
            return data
        var c = n - 1
        for (i <- n - 2 to 0 by -1) {
            // j-i + c <= cost // i <= j <= cost - c + i <= n-1
            // => j = i.max(cost - c + i)
            val j = i.max(cost - c + i).min(n - 1)
            rever(i, j, data)
            c += j - i
        }
        if (c != cost) {
            return Array.empty
        }
        data
    }

    def main(args: Array[String]): Unit = {
//        println(reverSortCost(Array(4, 2,1,3)))
//        println(reverSortCost(Array(1, 2)))
//        println(reverSortCost(Array(7, 6, 5, 4, 3, 2, 1)))

//        return

        for (n <- 2 to 100)
            for (cost <- 1 to 1000) {
                val data = reverToCost(n, cost)
                val rs = if (data.isEmpty) "IMPOSSIBLE" else data.mkString(" ")
                if (rs == "IMPOSSIBLE") {
                    println(n, cost, "IMPOSSIBLE")
                } else if (reverSortCost(data) != cost) {
                    println(n, cost)
                }
//                println(s"Case #${n * 10000 + cost}: $rs")
                //            println(s"Cost ${reverSortCost(data)} \t expect $cost")
            }
        return
        val t = readInt()
        for (i <- 1 to t) {
            val Array(n, cost) = readArrInt
            val data = reverToCost(n, cost)
            val rs = if (data.isEmpty) "IMPOSSIBLE" else data.mkString(" ")
            println(s"Case #$i: $rs")
//            println(s"Cost ${reverSortCost(data)} \t expect $cost")
        }
    }
}
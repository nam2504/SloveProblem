package NamLA.leetCode

object AdvantageShuffle {
    def advantageCount(a: Array[Int], b: Array[Int]): Array[Int] = {
        val sorted = collection.mutable.TreeMap[Int, Int]()
        a.foreach(i => sorted.put(i, sorted.getOrElse(i, 0) + 1))
        for (i <- b.indices.reverse) {
            val max = sorted.minAfter(b(i) + 1).getOrElse(sorted.head)._1
            a(i) = max
            sorted(max) -= 1
            if (sorted(max) == 0)
                sorted.remove(max)

        }
        a
    }

    def test(data: String): Unit = {
        val x = Helper.to2Arr(data)
        println(advantageCount(x._1, x._2).mkString(","))
    }

    def main(args: Array[String]): Unit = {
        test("[2,0,4,1,2]\n[1,3,0,0,2]")
    }
}

package NamLA.leetCode

object MinKBitFlips {
    def minKBitFlips1(data: Array[Int], k: Int): Int = {
        var count = 0
        var idx = 0
        while (idx <= data.length - k ) {
            if (data(idx) == 0) {
                count += 1
                for (i <- idx until idx + k) {
                    data(i) = 1 - data(i)
                }
            }
            idx += 1
        }
        for (i <- idx until data.length)
            if (data(i) == 0)
                return -1
        count
    }

    def minKBitFlips(A: Array[Int], K: Int): Int = {
        val queue = collection.mutable.Queue[Int]()
        var idx = 0
        var count = 0
        var sum = 0
        while (idx < A.length) {
            if (queue.nonEmpty && queue.head <= idx) {
                sum -= 1
                queue.dequeue()
            }
            // need to flip
            if ((A(idx) + sum) % 2 == 0) {
                if (idx + K > A.length)
                    return -1
                // flip
                count += 1
                sum += 1
                queue.enqueue(idx + K)
            }
            idx += 1
        }

        count
    }

    def main(args: Array[String]): Unit = {
        println(minKBitFlips(Helper.toArr("[0,0,0,1,0,1,1,0]"), 3))
    }
}

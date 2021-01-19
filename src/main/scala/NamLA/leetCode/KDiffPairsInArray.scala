package NamLA.leetCode

object KDiffPairsInArray {
    def findPairs(nums: Array[Int], k: Int): Int = {
        if (k == 0) {
            val data = nums.groupMapReduce[Int, Int]((k: Int) => k)(_ => 1)(_ + _)
            return data.count(_._2 > 1)
        }
        val set = nums.toSet
        set.count(n => (n >= k && set(n - k)) || (n < 0) && (n + k) < k && set(n + k))
    }
}

package NamLA.leetCode

object MaxNonOverlapping {

    def maxNonOverlapping(nums: Array[Int], target: Int): Int = {
        //
        val mapSum = collection.mutable.Map[Int, Int]()
        var sum, rs = 0
        mapSum.put(0, 0)

        for (n <- nums) {
            sum += n
            mapSum.get(sum - target) match {
                case Some(value) =>
                    rs = rs.max(value + 1)
                case None =>
            }
            mapSum.put(sum, rs)
        }
        rs
    }
}

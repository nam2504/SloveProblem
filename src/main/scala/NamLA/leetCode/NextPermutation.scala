package NamLA.leetCode

object NextPermutation {
    def nextPermutation(nums: Array[Int]): Unit = {
        def swap(x: Int, y: Int): Unit = {
            val tmp = nums(x)
            nums(x) = nums(y)
            nums(y) = tmp
        }
        var x = nums.indices.drop(1).findLast( i => nums(i) > nums(i - 1)).getOrElse(0)
        if (x > 0) {
            val minIndex = nums.indices.findLast(i => nums(i) > nums(x - 1)).get
            swap(x - 1, minIndex)
        }
        // reverse from x to end
        var end = nums.length - 1
        while (x < end) {
            swap(x, end)
            x += 1
            end -= 1
        }
    }

    def main(args: Array[String]): Unit = {
        test("[1,2,3]")
        test("[1,3,2]")
        test("[2,1,3]")
        test("[2,3,1]")
        test("[3,1,2]")
        test("[3,2,1]")
        test("[1,1,5]")
        test("[1]")
    }

    def test(data: String): Unit = {
        val arr = Helper.toArr(data)
        nextPermutation(arr)

        println(arr.mkString(","))
    }
}

package NamLA.leetCode.weeklyContent
import NamLA.leetCode.Helper._

object S232 {


    def test1: Unit = {

    }

    def test2: Unit = {

    }

    def diff(x : (Int, Int)) : Double = {
        ratio((x._1 + 1, x._2 + 1)) - ratio(x)
    }

    def ratio(x: (Int, Int)): Double =  x._1 * 1.0 / x._2
    def maxAverageRatio(classes: Array[Array[Int]], extraStudents: Int): Double = {
        var sum: Double = 0.0
        val q = collection.mutable.PriorityQueue.empty[(Int, Int)]((x: (Int, Int), y: (Int, Int)) => diff(x).compare(diff(y)))
        for (c <- classes) {
            val data = (c.head, c.last)
            val r = ratio(data)
            sum += r
            if (r != 1)
                q.enqueue(data)
        }
        if (q.isEmpty)
            return 1
        var n = extraStudents
        while (n > 0) {
            val data = q.dequeue()
            sum -= ratio(data)
            val nData = (data._1 + 1, data._2 + 1)
            sum += ratio(nData)
            q.enqueue(nData)
            n -= 1
        }
        sum / classes.length
    }
    def test3(data: String): Unit = {
        val (a, n) = toArrArrAndInt(data)
        println("----------")
        println(maxAverageRatio(a, n))
    }
    def maximumScore(nums: Array[Int], k: Int): Int = {
        val n = nums.length - 1
        var max = nums(k)
        var i = k
        var j = k
        var min = nums(k)
        while (i > 0 || j < n) {
            while (i > 0 && nums(i - 1) > min)
                i -= 1
            while (j < n && nums(j + 1) > min)
                j += 1
            max = max.max(min * (j - i + 1))
            if (i > 0 && (j == n || nums(i - 1) > nums(j + 1)) ) {
                i -= 1
                min = min.min(nums(i))
            } else if (j < nums.length - 1) {
                j += 1
                min = min.min(nums(j))
            }
        }
        max.max(min * (j - i + 1))
    }
    def test4: Unit = {
        def test(data: String): Unit = {
            val (a, i) = toArrAndInt(data)
            println(maximumScore(a, i))
        }
        test("[1,4,3,7,4,5]\n3") // 15
        test("[5,5,4,5,4,1,1,1]\n0") // 20

    }

    def findCenter(edges: Array[Array[Int]]): Int = {
        val count = Array.fill(edges.length + 1)(0)
        for (e <- edges) {
            count(e.head) += 1
            count(e.last) += 1
        }
        count.indices.maxBy(i => count(i))
    }

    def main(args: Array[String]): Unit = {

        test3("[[1,2],[3,5],[2,2]]\n2")// 0.78
        test3("[[2,4],[3,9],[4,5],[2,10]]\n4")
    }
}

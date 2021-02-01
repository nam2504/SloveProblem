package NamLA.leetCode

object Solution {
    import collection.mutable._

    def countConsistentStrings(allowed: String, words: Array[String]): Int = {
        val set = allowed.toSet
        words.count(i => i.forall(c => set(c)))
    }

    def getSumAbsoluteDifferences(nums: Array[Int]): Array[Int] = {
        val sum = nums.clone
        for (i <- 1 until sum.length)
            sum(i) += sum(i - 1)
        val rs = nums.clone
        rs(0) = sum.last - nums.head * sum.length
        for (i <- 1 until sum.length) {
            rs(i) = sum.last + (i + 1) * nums(i) - 2 * sum(i)  - nums(i) * (sum.length -1 - i)
        }
        rs
    }


    def stoneGameVI(aliceValues: Array[Int], bobValues: Array[Int]): Int = {
        var alice = aliceValues.sum
        for (i <- bobValues.indices) {
            bobValues(i) += aliceValues(i)
        }
        val sorted = bobValues.sorted.reverse
        for (i <- 1 until sorted.length by 2) {
            alice -= sorted(i)
        }
        alice.compareTo(0)
    }

    val MAX = 1000000007

    def concatenatedBinary(n: Int): Int = {
        var rs = 0L
        var cntBit = 0

        for (i <- 1 to n) {
            if (0 == ((i - 1) & i))
                cntBit += 1
            rs = (rs << cntBit)% MAX + i
            rs = rs % MAX
        }
        rs.toInt
    }


    def main(args: Array[String]): Unit = {
        println(MAX)
        println(concatenatedBinary(12)) //505379714
        return
        test(Array(2,3,5))
        test(Array(1,4,6,8,10))
        return
        test(Array(1, 3), Array(2, 1)) // 1
        test(Array(1, 2), Array(3, 1)) // 0
        test(Array(2,4,3), Array(1,6,7)) // -1
    }
    def test(a: Array[Int], b: Array[Int]) = {
        println(stoneGameVI(a, b))
    }
    def test(a: Array[Int]) = {
        //        println(stoneGameVI(a, b))
        println(getSumAbsoluteDifferences(a.sorted).mkString(","))
    }
}

package NamLA.leetCode

// HARD
object LeastOpsExpressTarget {
    def leastOpsExpressTarget(x: Int, target: Int): Int = {
        // prev : t = x^prev*(t1 + t2)?
        if (x > target)
            return Math.min(target * 2 - 1, (x - target) * 2)
        if (x == target) { // just push x at the end
            return 0
        }
        var sum = 0L + x
        var times = 0
        while (sum < target) {
            times += 1
            sum *= x
        }
        if (sum == target)
            return times
        // t = x^times - a = target || x^(times - 1) + b = target
        // => a = x^times - target ; b = target - x^(times - 1)
        var leastA, leastB = Int.MaxValue
        val a = sum - target
        if (a < target) {
            leastA = leastOpsExpressTarget(x, a.toInt) + times
        }
        val b = target - sum / x
        leastB = leastOpsExpressTarget(x, b.toInt) + times - 1
        leastA.min(leastB) + 1
    }

    def getSmallestString2(n: Int, k: Int): String = k match {
        case x if x >= 26 + n =>
            val max = (k + 1 - n) / 26
            getSmallestString(n - 1, k - 26 * max) + "z" * max
        case _ => if (n > 1) {
            "a" * (n - 1) + getSmallestString(1, k - n + 1)
        } else
            (k + 96).toChar.toString
    }

    def getSmallestString(n: Int, k: Int): String = {
        val z = (k - n) / 25
        if (z == n) return "z" * z
        val x = (k - n) % 25
        "a" * (n - 1 - z) + (x + 97).toChar + "z" * z
    }

    def main(args: Array[String]): Unit = {
//        println(getSmallestString(3, 27))
//        println(getSmallestString(5, 73))
        println(getSmallestString(5, 130))
        println(getSmallestString(50220, 1018738))
        return
        val f = (x: Int, target: Int) => println(leastOpsExpressTarget(x, target))
        //        f(3, 9) // 1
        //        f(3, 27) // 2
        //        f(3, 24) // 3
        f(3, 19) // 5
        //        f(5, 501) // 8
    }
}

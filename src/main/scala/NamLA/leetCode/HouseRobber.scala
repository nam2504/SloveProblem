package NamLA.leetCode

object HouseRobber {
    // rob basic
    def rob1(nums: Array[Int]): Int = {
        var soFar = 0
        var max = 0
        // Traverse all houses and update max
        nums.foreach(money => {
            val tmp = max
            max = Math.max(max, soFar + money)
            soFar = tmp
            1 == 1
        })
        max
    }
    // rob arranged in a circle (last next to head)
    def rob(nums: Array[Int]): Int = {
        rob1(nums.drop(1)).max(rob1(nums.dropRight(1)))
    }

    def removeDuplicateLetters(s: String): String = {
        var rs = ""

        for (c <- s.reverse) {
            if (!rs.contains(c)) {
                rs = c + rs
            } else {
                //
                val n = c + rs.filter(p => p != c)
                if (n < rs) {
                    rs = n
                }
            }
        }
        rs
    }

    def smallestSubsequence(s: String): String = {
        if (s.length <= 1)
            return s
        val count = Array.fill('z' - 'a')(0)
        for (c <- s)
            count(c - 'a') += 1

        //
        def findMinPos: Int = {
            var pos = 0
            for (i <- s.indices) {
                if (s(i) < s(pos)) {
                    pos = i
                }
                if (count(s(i) - 'a') <= 1) {
                    return pos
                } else {
                    count(s(i) - 'a') -= 1
                }
            }
            pos
        }
        val pos = findMinPos
        s(pos) + smallestSubsequence(s.substring(pos).replaceAll("" + s.charAt(pos), ""))
    }

    def main(args: Array[String]): Unit = {
        val f = (x:String)=> println(smallestSubsequence(x))

//        f("cbacdcbc") //acdb
//        f("abacb") //abc
//        f("accaccbbcc") //abc
//        f("leetcode")
        f("cbaacabcaaccaacababa") // abc
    }
}

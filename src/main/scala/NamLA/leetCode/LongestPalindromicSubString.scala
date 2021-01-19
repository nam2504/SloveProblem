package NamLA.leetCode

object LongestPalindromicSubString {
    def longestPalindrome(s: String): String = {
        @scala.annotation.tailrec
        def maxLength(x: Int, y: Int): String = {
            if (!(x > 0 && y < s.length - 1 && s(x - 1) == s(y + 1))) {
                return s.substring(x, y + 1)
            }
            maxLength(x - 1, y + 1)
        }

        var max = ""
        for (i <- s.indices) {
            if (i > 0 && s(i - 1) == s(i)) {
                val sub = maxLength(i - 1, i)
                if (sub.length > max.length) {
                    max = sub
                }
            }
            val sub = maxLength(i, i)
            if (sub.length > max.length) {
                max = sub
            }

        }
        max
    }

    def main(args: Array[String]): Unit = {
        val f: String => Unit = (x: String) => println(longestPalindrome(x))
//        f("babad")
        f("abbc")
    }
}

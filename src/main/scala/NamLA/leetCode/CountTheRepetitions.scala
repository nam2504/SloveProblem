package NamLA.leetCode

object CountTheRepetitions {
    def getMaxRepetitions(s1: String, n1: Int, s2: String, n2: Int): Int = {
        val setS2 = s2.toCharArray.toSet
        if (s1.exists(c => !setS2(c))) {
            return getMaxRepetitions(s1.filter(c => setS2(c)), n1, s2, n2)
        }

        //
        if (s1.length * n1 < s2.length * n2)
            return 0
        val data = Array.fill(n1)(0)
        var s1Idx = 0
        var s2Idx = 0
        var countM = 0
        var countN = 1
        while (countN <= n1 && s1Idx < s1.length) {
            if (s1(s1Idx) == s2(s2Idx)) {
                s2Idx += 1
                if (s2Idx == s2.length) {
                    countM += 1
                    s2Idx = 0
                }
            }
            s1Idx += 1
            if (s1Idx == s1.length) {
                data(countN) = countM
                s1Idx = 0
                if (s2Idx == 0) {
                    return (n1 / countN * countM + data(n1 % countN)) / n2
                }
                countN += 1
            }
        }

        countM / n2
    }

    def test(data: String): Unit = {
        val Array(s1: String, n1: String, s2: String, n2: String) = data.split("\n")
        println(getMaxRepetitions(s1.drop(1).dropRight(1), n1.toInt, s2.drop(1).dropRight(1), n2.toInt))
    }

    def main(args: Array[String]): Unit = {
//        test("\"acb\"\n4\n\"ab\"\n2")
//        println(longestValidParentheses(")(")) // 0
        println(longestValidParentheses("())")) // 2
//        println(longestValidParentheses("(()")) // 2
//        println(longestValidParentheses(")()())")) // 4
//        println(longestValidParentheses("()(()")) // 2
    }

    def longestValidParentheses(s: String): Int = {
        val stack = collection.mutable.Stack[Int]() // save all indices that cannot make a pair
        for (i <- s.indices) {
            // if can make a pair
            if (s(i) == ')' && stack.nonEmpty && s(stack.top) == '(') {
                stack.pop()
            } else {
                stack.push(i)
            }
        }
        if (stack.isEmpty)
            return s.length
        var max = 0
        var end = s.length
        while (stack.nonEmpty) {
            val start = stack.pop()
            max = max.max(end - start - 1)
            end = start
        }
        max.max(end)
    }


}

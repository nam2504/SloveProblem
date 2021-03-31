package NamLA.leetCode

object StampingTheSequence {

    def movesToStamp(stamp: String, target: String): Array[Int] = {
        val rs = collection.mutable.ArrayBuffer[Int]()
        val t = target.toCharArray
        val replaced = Array.fill(t.length)(false)
        def isMatch(start: Int): Boolean = {
            !replaced(start) && !stamp.indices.exists(i => t(i + start) != '*' && t(i + start) != stamp(i))
        }

        var replaceCount = 0
        def replace(index: Int): Unit = {
            replaced(index) = true
            rs.append(index)
            for (i <- stamp.indices if t(index + i) != '*') {
                replaceCount += 1
                t(index + i) = '*'
            }
        }

        while (replaceCount < target.length) {
            val replaceIndex = t.indices.dropRight(stamp.length - 1).find(isMatch)
            if (replaceIndex.isEmpty){
                return Array.empty
            }
            replace(replaceIndex.get)
        }
        rs.toArray.reverse
    }

    def test(s: String, t: String): Unit = {
        println(movesToStamp(s, t).mkString(","))
    }

    def main(args: Array[String]): Unit = {
        test("abc", "ababc")
        test("abca", "aabcaca")
    }
}

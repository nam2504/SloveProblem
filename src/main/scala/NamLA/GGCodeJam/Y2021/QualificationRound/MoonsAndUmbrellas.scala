package NamLA.GGCodeJam.Y2021.QualificationRound

object Solution {

    import GGCJHelper._

    def mUCost(x: Int, y: Int, data: String): Int = {
        def getCost(i: Int, j: Int): Int = {
            if (i == -1 || data(i) == data(j)) return 0
            if (data(i) == 'C')
                x
            else
                y
        }
        def calCost(i: Int, j: Int): Int = {
            if (i == -1 && j == data.length) {
                // ????????????
                return costAll(x, y, data.length)
            }
            if (i != -1 && j != data.length) {
                val l = j - i + 1
                if (data(i) == 'C' && data(j) == 'C') {
                    return cost2Same(x, y, l)
                } else if (data(i) == 'C' && data(j) == 'J') {
                    return cost2Diff(x, y, l)
                } else if (data(i) == 'J' && data(j) == 'J') {
                    return cost2Same(y, x, l)
                } else {
                    return cost2Diff(y, x, l)
                }
            }
            if ((i != -1 && data(i) == 'C') || (i == -1) && data(j) == 'J') {
                // C???? || ???J
                costOne(x, y, j + 1)
            } else {
                costOne(y, x, j + 1)
            }
        }
        var lastIndex = -1
        var i = 0
        var cost = 0
        while (i < data.length) {
            if (data(i) != '?') {
                if (lastIndex == i - 1) {
                    cost += getCost(lastIndex, i)
                } else {
                    // x ??? y
                    cost += calCost(lastIndex, i)
                }
                lastIndex = i
            }
            i += 1
        }
        if (data.last == '?')
            cost += calCost(lastIndex, data.length)
        cost
    }

    def costAll(x: Int, y: Int, length: Int): Int = {
        if (x >= 0 && y >= 0) return 0
        val z = x + y
        if (z >= 0) return x.min(y) // CJJ... | JC...
        // CJCJC || JCJCJC
        if (length % 2 == 0) {
            return z * length / 2 + x.min(y)
        }
        (z * length).min(x).min(y)
    }

    def cost2Same(x: Int, y: Int, length: Int): Int = {
        // X????X
        if (x >= 0 && y >= 0)
            return 0
        val z = x + y

        0
    }

    def cost2Diff(x: Int, y: Int, length: Int): Int = {
        // X???Y
        if (x >= 0 && y >= 0)
            return x
        0
    }

    def costOne(x: Int, y: Int, leng: Int): Int = {
        //X????
        if (x >= 0 && y >= 0)
            return 0
        0
    }

    def test(line: String): Int = {
        val seqs = line.split(" ")
        val x = seqs(0).toInt
        val y = seqs(1).toInt
        val data = seqs.last
        mUCost(x, y, data)
    }

    def main(args: Array[String]): Unit = {

        val t = readInt()
        for (i <- 1 to t) {
            val line = readLine
            val rs = test(line)
            println(s"Case #$i: $rs")
        }
    }
}

//        println(test("2 3 CJ?CC?")) // 5
//        println(test("4 2 CJCJ")) // 10
//        println(test("1 3 C?J")) // 1
//        println(test("2 5 ??J???")) // 0
//        println(test("2 -5 ??JJ??")) // -8
//        println(test("-2 -5 ??JJ??")) //-28
//        return
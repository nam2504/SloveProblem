package NamLA.leetCode

object WaysToFillArray {
    val max: Int = (10e9 + 7).toInt
    val primes = Array(2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97)
    //Stars and bars (combinatorics)
    val sBC = Array.fill(1015, 15)(1) //2^14 > 10^4
    calC

    def calC: Unit = {
        for (n <- 1 until 1015) sBC(n)(0) = 1

        for (n <- 1 until 1015)
            for (k <- 1 until 15)
                sBC(n)(k) = (sBC(n - 1)(k - 1) + sBC(n - 1)(k)) % max

    }

    def waysToFillArray(queries: Array[Array[Int]]): Array[Int] = {

        def f(n: Int, k: Int): Long = {
            var rs = 1L
            var v = k
            for (p <- primes if v % p == 0) {
                var i = 0
                while (v % p == 0) {
                    v = v / p
                    i += 1
                }
                rs = (rs * sBC(n + i - 1)(i)) % max
            }
            rs.toInt
        }

        queries.map {
            case Array(n, k) => (f(n, k) % max).toInt
        }
    }

    def main(args: Array[String]): Unit = {
        println(countOfAtoms("K4(ON(SO3)2)2")) //K4N2O14S4
        println(countOfAtoms("Mg(OH)2")) //H2MgO2
        println(countOfAtoms("Be32")) //Be32
    }

    def test(s: String): Unit = {
        val data = Helper.toArrArr(s)
        println(waysToFillArray(data).mkString(","))
    }

    def countOfAtoms(formula: String): String = {
        val count = collection.mutable.Map[String, Int]()
        val stack = collection.mutable.Stack[Int]()
        def getStack: Int = if (stack.isEmpty) 1 else stack.top
        var tmp = ""
        def convertTmp: Int = {
            val  v = if (tmp.isEmpty) 1 else tmp.toInt
            tmp = ""
            v
        }
        var name = ""
        formula.reverse.foreach{
            case d : Char if d.isDigit => tmp = d + tmp
            case ')' =>
                val cnt = getStack
                val cntTmp = convertTmp
                stack.push(cntTmp * cnt)
            case '(' =>
                stack.pop()
            case c: Char =>
                name = c + name
                if (c.isUpper) {
                    val cnt = getStack
                    val cntTmp = convertTmp
                    val oC = count.getOrElse(name, 0) + cnt * cntTmp
                    count.put(name, oC)
                    name = ""
                }
            case _ =>
        }
        count.toArray.map(i => s"${i._1}${if (i._2 > 1) s"${i._2}" else ""}").sorted.mkString("")
    }

    def maxChunksToSorted(arr: Array[Int]): Int = {
        val max = arr.clone
        for (i <- 1 until arr.length)
            max(i) = Math.max(max(i), max(i - 1))
        var i = arr.length - 1
        var min = arr.last
        var cnt = 1
        while (i > 0) {
            i -= 1
            if (max(i) <= min) {
                cnt += 1
            }
            min = Math.min(min, arr(i))
        }
        cnt
    }
}

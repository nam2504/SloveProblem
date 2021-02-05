package NamLA.leetCode.weeklyContent

import NamLA.leetCode.Helper
import NamLA.leetCode.Helper._

import scala.collection.convert.ImplicitConversionsToJava.`map AsJavaMap`
object Solution22522 {
    def maximumTime(time: String): String = {
        val seq = time.split(":")
        val hh = seq(0)
        val mm = seq(1)
        var rs = ""
        hh match {
            case "??" => rs += "23:"
            case ch if (ch(0) == '?' && ch(1) < '4') => rs += "2" + ch(1) + ":"
            case ch if (ch(0) == '?' && ch(1) >= '4') => rs += "1" + ch(1) + ":"
            case ch if (ch(1) == '?' && ch(0) < '2') => rs += ch(0) + "9" + ":"
            case ch if (ch(1) == '?' && ch(0) == '2') => rs += hh(0) + "3" + ":"
            case _ =>  rs += hh + ":"
        }

        mm match {
            case "??" => rs += "59"
            case ch if (ch(0) == '?') => rs += "5" + ch(1)
            case ch if (ch(1) == '?') => rs += ch(0) + "9"
            case _ => rs += mm
        }

        rs
    }

    def test1: Unit = {
        println(maximumTime("?1:?0"))
        println(maximumTime("?4:3?"))
        println(maximumTime("??:??"))
        println(maximumTime("1?:30"))
        println(maximumTime("2?:30"))
        println(maximumTime("0?:30"))
    }

    def minCharacters(a: String, b: String): Int = {
        var rs = 0

        val cA = a.groupMapReduce((c: Char) => c)(_ => 1)(_ + _)
        val cB = b.groupMapReduce((c: Char) => c)(_ => 1)(_ + _)
        // distict
        val total = cA.values.sum + cB.values.sum
        var max = 0
        cA.foreach( k => max = max.max(k._2 + cB.getOrElse(k._1, 0)))
        rs = total - max
        //
        val minA = cA.keys.min
        val minB = cB.keys.min
        val convertB = cB.filterKeys(c => c >= minA).values.sum
        val convertA = cA.filterKeys(c => c >= minB).values.sum

        val maxA = cA.keys.max
        val maxB = cB.keys.max
        val convertB2 = cB.filterKeys(c => c <= maxA).values.sum
        val convertA2 = cA.filterKeys(c => c <= maxB).values.sum
        rs.min(convertA).min(convertB).min(convertA2).min(convertB2)
    }
    def test2: Unit = {
        println(minCharacters("aba", "caa")) // 2
        println(minCharacters("dabadd", "cda")) //3
        println(minCharacters("acac",
        "bd")) // 1
    }

    def kthLargestValue(matrix: Array[Array[Int]], k: Int): Int = {
        val xor = matrix.clone()
        val m = matrix.length
        val n = matrix.head.length
        xor(0)(0) = matrix(0)(0)
        for (i <- 1 until n) {
            xor(0)(i) = xor(0)(i - 1) ^ matrix(0)(i)
        }
        for (i <- 1 until m) {
            xor(i)(0) = xor(i - 1)(0) ^ matrix(i)(0)
        }
        for (i <- 1 until m) {
            for (j <- 1 until n) {
                xor(i)(j) = xor(i)(j - 1) ^ xor(i - 1)(j) ^ matrix(i)(j) ^ xor(i - 1)(j - 1)
            }
        }
        xor.flatten.sorted.reverse(k - 1)
    }

    def test3: Unit = {
        val f = (a: Array[Array[Int]], k: Int) => println(kthLargestValue(a, k))
        f(Array(Array(8,10,5,8,5,7,6,0,1,4,10,6,4,3,6,8,7,9,4,2)),
            2) // 14

return
        f(Array(Array(5,2),Array(1,6)), 1) //
        f(Array(Array(5,2),Array(1,6)), 2) //
        f(Array(Array(5,2),Array(1,6)), 3) //
        f(Array(Array(5,2),Array(1,6)), 4) //


    }

    def minimumBoxes(n: Int): Int = {
        var total = 1
        var floor = 1
        var i = 1
        while (total < n) {
            i += 1
            floor += i
            total += floor
            1 == 1
        }
        if (n < total - floor + i) {
            return floor - 2
        }
        if (n < total - i + 1) {
            return floor - 1
        }

        floor
    }

    def test4 {
//        println(minimumBoxes(15)) // 9
        println(minimumBoxes(126)) // 39
        return
        println(minimumBoxes(3))
        println(minimumBoxes(4))
        println(minimumBoxes(10))
    }

    def main(args: Array[String]): Unit = {
//        test1
//        test2

//        test3//

        test4

    }
}

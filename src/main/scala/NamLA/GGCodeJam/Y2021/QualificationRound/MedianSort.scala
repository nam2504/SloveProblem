package NamLA.GGCodeJam.Y2021.QualificationRound
import NamLA.GGCodeJam.Y2021.GGCJHelper._
object MedianSort {

    import scala.collection.mutable.ArrayBuffer

    var n = 0
    var q = 0
    var i = 0
    var data: ArrayBuffer[Int] = _

    def setMedian(i: Int, j: Int, next: Int, median: Int, from: Int, to: Int): Unit = {
        if (median == i) {
            // from, k, i, j // or k, from, i, j
            if (from == i) {
                data.insert(data.indexOf(from), next)
                return
            }
            getAsk(from, i, next)
            return
        }
        if (median == j) {
            // i, j, k, to // or i, j, to, k
            if (j == to) {
                data.insert(data.indexOf(to) + 1, next)
                return
            }
            getAsk(j, to, next)
            return
        }

        // i, next, j
        if (data.indexOf(i) + 1 >= data.indexOf(j)) {
            data.insert(data.indexOf(j), next)
            return
        }
        getAsk(i, j, next)
    }

    def find2Mid(from: Int, to: Int): (Int, Int) = {
        val iF = data.indexOf(from)
        val iT = data.indexOf(to)
        val num = iT - iF + 1
        if (num == 2) return (from, to)
        if (num == 3) return (from, data(iF + 1))
        if (num == 4) return (data(iF + 1), data(iF + 2))
        val step = ((iT - iF) / 3).toInt //
        (data(data.indexOf(from) + step), data(data.indexOf(to) - step))
    }

    def getAsk(from: Int = data.head, to: Int = data.last, next: Int = data.length + 1): Unit = {
        val mid = find2Mid(from, to)
        val median = ask(mid._1, mid._2, next)
        setMedian(mid._1, mid._2, next, median, from, to)
    }

    def clear: Unit = {
        data = new ArrayBuffer[Int](n)
        data.append(1)
        data.append(2)
    }

    def ask(i: Int,j: Int, k: Int): Int = {
        qi += 1

        // Testing
        val dataIndex = Array(RS.indexOf(i), RS.indexOf(j), RS.indexOf(k)).sorted
        return RS(dataIndex(1))

        println(s"$i $j $k")
        val median = readInt()
        median
    }

    var qi = 0
    def getTask: Unit = {
        qi = 0
        while (qi < q && data.length < n) {
            getAsk()
        }
    }

    var RS: Array[Int] = _
    def test: Unit = {
        n = 50
        q = 170
        clear
        RS = scala.util.Random.shuffle(Range(1, n + 1).iterator).toArray

        getTask
        if (RS.mkString(" ") == data.mkString(" ") || RS.mkString(" ") == data.reverse.mkString(" ")) {
            return
        }
        println(RS.mkString(" "))
        println(data.mkString(" "))
        println()
    }

    def main(args: Array[String]): Unit = {
        for (_ <- 1 to 100000)
            test
        return
        val Array(t, _n, _q) = readArrInt
        n = _n
        q = _q

        for (_ <- 1 to t) {
            clear
            getTask
            println(data.mkString(" "))
            val rs = readInt()
            if (rs == -1) return
        }
    }
}

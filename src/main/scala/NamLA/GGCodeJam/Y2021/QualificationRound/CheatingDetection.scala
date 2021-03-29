package NamLA.GGCodeJam.Y2021.QualificationRound

object CheatingDetection {

    import GGCJHelper._

    def cheat(p: Int, data: Array[Array[Boolean]]): Int = {
        val count = Array.fill(100)(0)
        val qCount = Array.fill(10000)(0)
        for (c <- data.indices) {
            for (q <- data(c).indices) {
                if (data(c)(q)) {
                    qCount(q) += 1
                    count(c) += 1
                }
            }
        }

        val hardQ = qCount.indices.filter(q => qCount(q) < 50).toArray
        val canCheat = data.indices.filter(i => count(i) >= 10000 * 0.5)

        val scoreOfHQ = data.map(ans => hardQ.count(q => ans(q)))

//        for (c <- canCheat) {
//            var total = 0
//            var a = 0
//            for (q <- data(c).indices if qCount(q) <= 25) {
//                total += 1
//                if (data(c)(q)) {
//                    a += 1
//                }
//            }
//
//        }

        def isCheater(i: Int): Boolean = {
            scoreOfHQ(i) * 100.0 / hardQ.length > 11.92
        }

        canCheat.count(isCheater)
    }

    def readData: Array[Array[Boolean]] = {
        val data = Array.fill(100)(Array.empty[Boolean])
        for (i <- 0 to 99) {
            data(i) = readArrBoolean
        }
        data
    }

    def readTest(): Array[Array[Boolean]] = {
        val file = "E:\\namla\\GGCodeJam\\cheating_detection_sample_ts1_input.txt"
        io.Source.fromFile(file).getLines().drop(2).map(i => i.toCharArray.map(_ == '1')).toArray
    }

    def test(): Unit = {
        val data = readTest()
        println(cheat(0, data))
    }

    def main(args: Array[String]): Unit = {
        test
        return
        val t = readInt()
        for (i <- 1 to t) {
            val p = readInt()
            val data = readData
            val rs = cheat(p, data)
            println(s"Case #$i: $rs")
        }
    }
}
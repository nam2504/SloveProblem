package NamLA.leetCode.weeklyContent

import NamLA.leetCode.Helper


object S227 {

    def countBalls(lowLimit: Int, highLimit: Int): Int = {
        val map = collection.mutable.Map[Int, Int]()
        var max, maxC = 0
        for (i <- lowLimit to highLimit) {
            val box = i.toString.toCharArray.map(c => c.toInt - 48).sum
            val c = map.getOrElse(box, 0) + 1
            map.put(box, c)
            if (c > maxC) {
                maxC = c
                max = box
            }
        }
        maxC
    }

    def restoreArray(adjacentPairs: Array[Array[Int]]): Array[Int] = {
        val map: collection.mutable.Map[Int, collection.mutable.ArrayBuffer[Int]] = collection.mutable.Map()
        for (Array(a, b) <- adjacentPairs) {
            map.getOrElseUpdate(a, collection.mutable.ArrayBuffer[Int]()).append(b)
            map.getOrElseUpdate(b, collection.mutable.ArrayBuffer[Int]()).append(a)
        }
        val rs = collection.mutable.ArrayBuffer[Int]()
        var head = map.find(i => i._2.length < 2).get._1
        while (map.contains(head)) {
            rs.append(head)
            val next = map(head)
            map.remove(head)
            head = next.find(i => map.contains(i)).getOrElse(head)
        }
        rs.toArray
    }

    def canEat(candiesCount: Array[Int], queries: Array[Array[Int]]): Array[Boolean] = {
        val sum = candiesCount.map(i => 0L + i)
        for (i <- 1 until candiesCount.length) sum(i) += sum(i - 1)

        def eat(t: Int, d: Int, c: Int): Boolean = {
            // eat 1 each day can reach t ?
            val last = if (t > 0) sum(t - 1) else 0

            1L * d * c >= last - (c - 1) && d <= sum(t) - 1
        }

        queries.map {
            case Array(t, d, c) =>
                eat(t, d, c)
        }
    }


    def main(args: Array[String]): Unit = {
        //true
        test("[5215,14414,67303,93431,44959,34974,22935,64205,28863,3436,45640,34940,38519,5705,14594,30510,4418,87954,8423,65872,79062,83736,47851,64523,15639,19173,88996,97578,1106,17767,63298,8620,67281,76666,50386,97303,26476,95239,21967,31606,3943,33752,29634,35981,42216,88584,2774,3839,81067,59193,225,8289,9295,9268,4762,2276,7641,3542,3415,1372,5538,878,5051,7631,1394,5372,2384,2050,6766,3616,7181,7605,3718,8498,7065,1369,1967,2781,7598,6562,7150,8132,1276,6656,1868,8584,9442,8762,6210,6963,4068,1605,2780,556,6825,4961,4041,4923,8660,4114]\n[[[91,244597,840227137]]]")

        return

        test("[7,4,5,3,8]\n[[0,2,2],[4,2,4],[2,13,1000000000]]")
        test("[5,2,6,4,1]\n [[3,1,2],[4,10,3],[3,10,100],[4,100,30],[1,3,1]]")

        //        test("[16,38,8,41,30,31,14,45,3,2,24,23,38,30,31,17,35,4,9,42,28,18,37,18,14,46,11,13,19,3,5,39,24,48,20,29,4,19,36,11,28,49,38,16,23,24,4,22,29,35,45,38,37,40,2,37,8,41,33,8,40,27,13,4,33,5,8,14,19,35,31,8,8]\n[[35,669,5],[72,822,74],[47,933,94],[62,942,85],[42,596,11],[56,1066,18],[54,571,45],[39,890,100],[3,175,26],[48,1489,37],[40,447,52],[30,584,7],[26,1486,38],[21,1142,21],[9,494,96],[56,759,81],[13,319,16],[20,1406,57],[11,1092,19],[24,670,67],[38,1702,33],[5,676,32],[50,1386,77],[36,1551,87],[29,1445,13],[58,977,13],[7,887,64],[37,1396,23],[0,765,69],[40,1083,86],[43,1054,49],[48,690,92],[28,1201,56],[47,948,43],[57,233,25],[32,1293,65],[0,1646,34],[43,1467,39],[39,484,23],[21,1576,69],[12,1222,68],[9,457,83],[32,65,9],[10,1424,42],[35,534,3],[23,83,22],[33,501,33],[25,679,51],[2,321,42],[1,240,68],[7,1297,42],[45,480,72],[26,1472,9],[6,649,90],[26,361,57],[49,1592,7],[11,158,95],[35,448,24],[41,1654,10],[61,510,43],[31,1230,95],[11,1471,12],[37,43,84],[56,1147,48],[69,1368,65],[22,170,24],[56,192,80],[34,1207,69],[1,1226,22],[37,1633,50],[11,98,58],[17,125,13],[0,1490,5],[37,1732,43],[45,793,14],[16,578,72],[50,241,78]]")
        test("[16,38,8,41,30,31,14,45,3,2,24,23,38,30,31,17,35,4,9,42,28,18,37,18,14,46,11,13,19,3,5,39,24,48,20,29,4,19,36,11,28,49,38,16,23,24,4,22,29,35,45,38,37,40,2,37,8,41,33,8,40,27,13,4,33,5,8,14,19,35,31,8,8]\n[[0,765,69],[40,1083,86],[43,1054,49]]")
        //[true,true,true,true,true,true,true,true,false,false,true,true,false,false,false,true,true,false,false,false,false,false,false,false,false,true,false,false,false,false,false,true,false,true,true,false,false,false,true,false,false,false,false,false,true,true,true,false,false,false,false,true,false,false,true,false,true,true,false,true,false,false,true,true,true,true,true,false,false,false,true,true,false,false,true,false,true]
        return
        val f = (data: String) => println(restoreArray(Helper.toArrArr(data)).mkString(","))
        f("[[2,1],[3,4],[3,2]]")
        f("[[4,-2],[1,4],[-3,1]]")
    }

    def test(data: String) = {
        val seq = data.split("\n")
        test2(seq(0), seq(1))
    }

    def test2(a: String, b: String): Unit = {
        println(canEat(Helper.toArr(a), Helper.toArrArr(b)).mkString(","))
    }

    def checkPartitioning(s: String): Boolean = {
        val map = collection.mutable.Map[(Int, Int), Boolean]()

        def check(i: Int, j: Int = s.length - 1): Boolean = {
            if (i == j) return true
            map.get((i, j)) match {
                case Some(x) => return x
                case _ =>
            }

            val rs = check2(i, j)
            map.put((i, j), rs)
            rs
        }

        def check2(i: Int, j: Int): Boolean = {
            var start = i
            var end = j
            while (start < end) {
                if (s(start) != s(end))
                    return false
                start += 1
                end -= 1
            }
            true
        }

        def split(start: Int = 0, c: Int = 3): Boolean = {
            if (c == 1) {
                return check(start)
            }
            for (i <- start until s.length) {
                if (check(start, i)) {
                    if (split(i + 1, c - 1)) {
                        return true
                    }
                }
            }
            false
        }

        split()
    }
}


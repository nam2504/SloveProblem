package NamLA.leetCode

import scala.collection.mutable


class DesignUndergroundSystem {

}

import scala.collection.mutable
class UndergroundSystem() {
    // S => Id => Array[time]
    val userCheckIns = mutable.Map[Int, (String, Int)]()
    val historyTravel = mutable.Map[(String, String), mutable.ArrayBuffer[Int]]()


    def checkIn(id: Int, stationName: String, t: Int) {
        userCheckIns.put(id, (stationName, t))
    }

    def checkOut(id: Int, stationName: String, t: Int) {
        val (inSN, inT) = userCheckIns(id)
        val joinT = t - inT
        val his = historyTravel.getOrElseUpdate((inSN, stationName), mutable.ArrayBuffer[Int]())
        his.append(joinT)
    }

    def getAverageTime(startStation: String, endStation: String): Double = {
        val his = historyTravel.getOrElse((startStation, endStation), mutable.ArrayBuffer[Int]())
        if (his.isEmpty) return 0
        his.sum * 1.0 / his.length
    }

}

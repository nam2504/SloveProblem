package NamLA.GGCodeJam.Y2021.QualificationRound

import scala.reflect.ClassTag

object GGCJHelper {
    def readLine: String = io.StdIn.readLine()
    def readInt(): Int = readLine.toInt
    def readArrInt: Array[Int] = readArr(_.toInt)
    def readArrBoolean: Array[Boolean] = readArr(_.toBoolean)
    def readArr[T](cvt: String => T)(implicit tag: ClassTag[T]): Array[T] = readLine.split(" ").map(cvt)
}

package NamLA

/**
  * Hello world!
  *
  */
object App {

    def originalDigits(s: String): String = {
        val count = collection.mutable.Map() ++ s.toCharArray.groupMapReduce[Char, Int]((c: Char) => c)(_ => 1)(_ + _)
        def isContain(num: String): Boolean = num.toCharArray.forall(count.getOrElse(_, 0) > 0)
        def rm(num: String): Unit = num.toCharArray.foreach(ch => {
//            val c = count(ch) - 1
            count(ch) -= 1
        })
        var rs = collection.mutable.ArrayBuffer[Int]()

        val digit = Array(("zero", 0), ("eight", 8) , ("two", 2) , ("three", 3),  ("four", 4), ("five", 5), ("one", 1), ("seven", 7), ("six", 6), ("nine", 9))
        for ((d, num) <- digit) {
            while (isContain(d)) {
                rs += num
                rm(d)
            }
        }
        rs.sorted.mkString("")
    }

    def main(args: Array[String]): Unit = {
        println("Hello World!")
        println(originalDigits("owoztneoer"))
        println(originalDigits("zeroonetwothreefourfivesixseveneightnine"))
    }
}

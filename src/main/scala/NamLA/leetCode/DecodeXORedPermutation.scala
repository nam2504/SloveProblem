package NamLA.leetCode

object DecodeXORedPermutation {
    def decode(encoded: Array[Int]): Array[Int] = {
        val xorAll = (1 to encoded.length + 1).foldLeft(0)(_ ^ _)
        val xorWithoutHead = encoded.indices.filter(i => i % 2 == 1).foldLeft(0)(_ ^ encoded(_))
        val head = xorAll ^ xorWithoutHead
        val ab = collection.mutable.ArrayBuffer[Int]()
        ab.append(head)
        for (e <- encoded) {
            ab.append(ab.last ^ e)
        }
        ab.toArray
    }

    def main(args: Array[String]): Unit = {
        test("[3,1]") //1,2,3
        test("[6,5,4,6]") //[2,4,1,5,3]
    }

    def test(data: String): Unit = {
        println(decode(Helper.toArr(data)).mkString(","))
    }
}

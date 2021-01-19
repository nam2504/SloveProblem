package NamLA.leetCode

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class KDiffPairsInArrayTest extends AnyFlatSpec with should.Matchers {
    val f: (Array[Int], Int) => Int = KDiffPairsInArray.findPairs
    it should "TestNormal" in {
        f(Array(1,2,3,1,1), 0) should be (2)
        f(Array(1,2,3,4,5), 1) should be (4)
        f(Array(-1,-2,-3,-4,-5), 1) should be (4)
    }
}

package NamLA.leetCode

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class MaxNonOverlappingTest extends AnyFlatSpec  with should.Matchers {

    "MaxNonOverlapping" should "TestNormal" in {
        val f: (Array[Int], Int) => Int = MaxNonOverlapping.maxNonOverlapping
        f(Array(1,1,1,1,1), 2) should be (2)
        f(Array(-1,3,5,1,4,2,-9), 6) should be (2)
        f(Array(-2,6,6,3,5,4,1,2,8), 10) should be (3)
        f(Array(0,0,0), 0) should be (3)
    }
}

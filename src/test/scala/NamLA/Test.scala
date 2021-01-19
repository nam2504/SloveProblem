package NamLA

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

object T {
    def sum(x: Int, y: Int): Int = {
        if (x >= 100) return x
        x + y
    }
}

class Test extends AnyFlatSpec with should.Matchers {

    it should "Sum two value" in {
        T.sum(1, 2) should be (3)
        T.sum(10, 20) should be (30)
        T.sum(100, 200) should be (300)
    }

}

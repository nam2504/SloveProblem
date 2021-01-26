package NamLA.leetCode


class CustomStack(_maxSize: Int) {
    val all = Array.fill(_maxSize)(-1)
    var size = 0
    def push(x: Int) {
        if (size >= _maxSize)
            return
        all(size) = x
        size += 1
    }

    def pop(): Int = {
        if (size == 0)
            return -1
        size -= 1
//        println(all(size))
        all(size)
    }

    def increment(k: Int, `val`: Int) {
        for (i <- 0 until k.min(size)) {
            all(i) += `val`
        }
    }
}

object TestCustomStack {

    def main(args: Array[String]): Unit = {
        println(search(Array(5,1,2,3,4), 1)) // 1
        println(search(Array(4,5,6,7,0,1,2), 0)) //4
        println(search(Array(4,5,6,7,8,1,2,3), 8 ))//4
//        return
        val data= Array(4,5,6,7,0,1,2)
        for (i <- data) {
            println(search(data, i), data.indexOf(i))
        }
        println(search(data, 3), data.indexOf(3))

    }

    def search(nums: Array[Int], target: Int): Int = {
        @annotation.tailrec
        def s(i: Int = 0, j: Int = nums.length - 1): Int = {
            if (nums(i) == target)
                return i
            if (nums(j) == target)
                return j
            if (i == j || i == j - 1){
                return -1
            }
            val mid = (i + j) / 2
            if (nums(mid) == target) {
                return mid
            }
            if (nums(i) < nums(mid)) {
                // 1,2,3,4
                if (nums(mid) < target) {
                    s(mid + 1, j)
                } else {
                    s(i, mid - 1)
                }
            } else {
                // 3,4,1,2
                if (nums(mid) < target) {
                    if (nums(j) < target)
                        s(i, mid - 1)
                    else
                        s(mid + 1, j)
                } else {
                    if (nums(i) < target && nums(i) < nums(mid))
                        s(i, mid - 1)
                    else
                        s(mid + 1, j)
                }
            }

        }
        s()
    }
}
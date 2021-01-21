package NamLA.leetCode

object CalculateString {
    // + - * /
    def calculate2(data: String): Int = {
        val stack = collection.mutable.Stack[Int]()
        val s = data + '+'
        var num = 0
        var sign = '+'
        s.foreach {
            case c if c.isDigit =>
                val d = c - '0'
                num = num * 10 + d
            case ' ' =>
            case c =>
                sign match {
                    case '+' => stack.push(num)
                    case '-' => stack.push(-num)
                    case '*' => stack.push(stack.pop() * num)
                    case '/' => stack.push(stack.pop() / num)
                    case _ =>
                }
                sign = c
                num = 0
        }
        stack.sum
    }

    def calculate(data: String): Int = {
        val s = data.trim
        if (s.contains('+')) {
            val seq = s.split('+')
            return seq.map(s =>
                calculate(s)
            ).sum
        }
        if (s.contains('-')) {
            val seq = s.split('-')
            return seq.map(s =>
                calculate(s)
            ).reduce(_ - _)
        }
        for (i <- s.indices.reverse) {
            lazy val a = calculate(s.substring(0, i))
            lazy val b = calculate(s.substring(i + 1))
            if (s(i) == '*')
                return a * b
            if (s(i) == '/')
                return a / b
        }

        s.toInt
    }

    def main(args: Array[String]): Unit = {
        val f : String => Unit = x => println(calculate(x))

        f("14/3*2") // 8
        return
        f("3")  //3
        f("3+2*2") // 7
        f("3/2") // 1
        f(" 3+5 / 2 ") // 5
    }
}

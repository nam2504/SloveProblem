package NamLA.leetCode

object QQ {
    def numUniqueEmails(emails: Array[String]): Int = {
        emails.map(normalize).toSet.size
    }

    def normalize(email: String): String = {
        val Array(localName: String, domain: String) = email.split("@")
        //validLocalName
        val validLocalName = localName.split('+').head.replace(".", "")
        s"$validLocalName@$domain"
    }

    def test1(data: String): Unit = {
        println(numUniqueEmails(Helper.toArrString(data)))
    }

    def totalFruit(tree: Array[Int]): Int = {
        val types = collection.mutable.Set[Int]()
        val count = Array.fill(tree.length + 1)(0)
        var start = 0
        var end = 0
        var max = 0
        def pick(i: Int): Boolean = {
            val t = tree(i)
            if (!types(t) && types.size == 2) {
                return false
            }
            count(t) += 1
            types.add(t)
            true
        }
        pick(start)
        while (end < tree.length - 1) {
            if (pick(end + 1)) {
                end += 1
                max = max.max(end - start + 1)
            } else {
                count(tree(start)) -= 1
                if (count(tree(start)) == 0) {
                    types.remove(tree(start))
                }
                start += 1
            }
        }
        max.max(end - start + 1)
    }

    def test(data: String): Unit =
        println(totalFruit(Helper.toArr(data)))

    def main(args: Array[String]): Unit = {
        test("[3,3,3,1,2,1,1,2,3,3,4]") //5
//        return
        test("[1,2,1]") // 3

        test("[0,1,2,2]") // 3
        test("[1,2,3,2,2]") //4


//        test1("[\"test.email+alex@leetcode.com\",\"test.e.mail+bob.cathy@leetcode.com\",\"testemail+david@lee.tcode.com\"]") // 2
//        test1("[\"a@leetcode.com\",\"b@leetcode.com\",\"c@leetcode.com\"]") // 3
    }
}

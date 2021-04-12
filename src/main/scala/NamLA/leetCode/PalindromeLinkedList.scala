package NamLA.leetCode

import NamLA.leetCode.Helper._

object PalindromeLinkedList {

    def isPalindrome(head: ListNode): Boolean = {
        var left = head
        def check(right: ListNode = head.next): Boolean = {
            if (right == null) return true
            if (!check(right.next) || left.x != right.x) {
                return false
            }
            left = left.next
            true
        }
        head == null || check()
    }
    val vowels = Set('a', 'e', 'i', 'o','u')
    def halvesAreAlike(s: String): Boolean = {
        val (l,r) = s.toLowerCase().splitAt(s.length / 2)
        val count = collection.mutable.Map[Char, Int]()
        l.filter(vowels.contains).foreach(ch => {
            val c = count.getOrElse(ch, 0)
            count.put(ch, c + 1)
        })
        r.filter(vowels.contains).foreach(ch => {
            val c = count.getOrElse(ch, 0)
            count.put(ch, c - 1)
        })
        !count.values.exists(i => i != 0)
    }

    def main(args: Array[String]): Unit = {
        val node = ListNode(Array(1,2,3,4,5))
        val rv = ListNode.revere(node)
        rv.printData()
    }
}

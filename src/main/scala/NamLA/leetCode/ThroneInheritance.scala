package NamLA.leetCode

import scala.collection.mutable.ArrayBuffer

class ThroneInheritance(_kingName: String) {
    class Person(val name: String, val child: ArrayBuffer[Person] = new ArrayBuffer(),var isLive : Boolean = true)
    val map = collection.mutable.Map[String, Person]()
    val king = new Person(_kingName)
    map.put(_kingName, king)
    def birth(parentName: String, childName: String) {
        val child = new Person(childName)
        map.put(childName, child)
        val parent = map(parentName)
        parent.child.append(child)
    }

    def death(name: String) {
        map(name).isLive = false
    }

    def getInheritanceOrder(): List[String] = {
        val names = new ArrayBuffer[String]()
        def f(p: Person): Unit = {
            if (p.isLive)
                names.append(p.name)
            p.child.foreach(f)
        }
        f(king)
        names.toList
    }
}

object TestTh {

    def isValid(s: String): Boolean = {
        val stack = collection.mutable.Stack[Char]()
        for (c <- s){
            c match {
                case ')' =>
                    if (stack.isEmpty || stack.pop() != '(')
                        return false
                case ']' =>
                    if (stack.isEmpty || stack.pop() != '[')
                        return false
                case '}' =>
                    if (stack.isEmpty || stack.pop() != '{')
                        return false
                case _ => stack.push(c)
            }
        }
        stack.isEmpty
    }
    def main(args: Array[String]): Unit = {
        println(isValid("()[]{}"))
        return
        val f = (x: List[String]) => println(x.mkString(","))

        val t = new ThroneInheritance("king"); // order: king
        t.birth("king", "andy"); // order: king > andy
        t.birth("king", "bob"); // order: king > andy > bob
        t.birth("king", "catherine"); // order: king > andy > bob > catherine
        t.birth("andy", "matthew"); // order: king > andy > matthew > bob > catherine
        t.birth("bob", "alex"); // order: king > andy > matthew > bob > alex > catherine
        t.birth("bob", "asha"); // order: king > andy > matthew > bob > alex > asha > catherine
        f(t.getInheritanceOrder()) // return ["king", "andy", "matthew", "bob", "alex", "asha", "catherine"]
        t.death("bob"); // order: king > andy > matthew > bob > alex > asha > catherine
        f(t.getInheritanceOrder()); // return ["king", "andy", "matthew", "alex", "asha", "catherine"]
    }
}
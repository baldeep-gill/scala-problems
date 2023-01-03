// Main Part 3 about Evil Wordle
//===============================


object M2 { 

import io.Source
import scala.util._

// ADD YOUR CODE BELOW
//======================


//(1)
def get_wordle_list(url: String) : List[String] = {
    Try(Source.fromURL(url).getLines().toList).getOrElse(Nil)
}

// val secrets = get_wordle_list("https://nms.kcl.ac.uk/christian.urban/wordle.txt")
// secrets.length // => 12972
// secrets.filter(_.length != 5) // => Nil

//(2)
def removeN[A](xs: List[A], elem: A, n: Int) : List[A] = {
    xs match {
        case Nil => List()
        case head :: tail if !xs.contains(elem) => xs
        case head :: tail if n >= xs.count(_ == elem) => xs.filter(_ != elem)
        case head :: tail if n > 0 && head != elem
            => head::removeN(tail, elem, n)
        case head :: tail if n > 0 && head == elem
            => removeN(tail, elem, n - 1)
        case _ if n == 0 => xs
    }
}


// removeN(List(1,2,3,2,1), 3, 1)  // => List(1, 2, 2, 1)
// removeN(List(1,2,3,2,1), 2, 1)  // => List(1, 3, 2, 1)
// removeN(List(1,2,3,2,1), 1, 1)  // => List(2, 3, 2, 1)
// removeN(List(1,2,3,2,1), 0, 2)  // => List(1, 2, 3, 2, 1)

// (3)
abstract class Tip
case object Absent extends Tip
case object Present extends Tip
case object Correct extends Tip


def pool(secret: String, word: String) : List[Char] = {
    def helper(str1: List[Char], str2: List[Char]) : List[Char] = str1 match {
        case Nil => List()
        case head :: tail if head == str2.head => helper(tail, str2.tail)
        case head :: tail if head != str2.head => head::helper(tail, str2.tail)
        case _ if str2 == Nil => str1
    }
    helper(secret.toList, word.toList)
}

def aux(secret: List[Char], word: List[Char], pool: List[Char]) : List[Tip] = {
    def helper(secret: List[Char], word: List[Char], pool: List[Char], tips: List[Tip] = Nil) : List[Tip] = word match {
        case head :: tail if head == secret.head => helper(secret.tail, tail, pool, Correct::tips)
        case head :: tail if pool.contains(head) => helper(secret.tail, tail, removeN(pool, head, 1), Present::tips)
        case head :: tail => helper(secret.tail, tail, pool, Absent::tips)
        case _ if word == Nil => tips.reverse
        case Nil => List()
    }
    helper(secret, word, pool)
}

def score(secret: String, word: String) : List[Tip] = {
    aux(secret.toList, word.toList, pool(secret, word))
}


// score("chess", "caves") // => List(Correct, Absent, Absent, Present, Correct)
// score("doses", "slide") // => List(Present, Absent, Absent, Present, Present)
// score("chess", "swiss") // => List(Absent, Absent, Absent, Correct, Correct)
// score("chess", "eexss") // => List(Present, Absent, Absent, Correct, Correct)

// (4)
def eval(t: Tip) : Int = ???

def iscore(secret: String, word: String) : Int = ???

//iscore("chess", "caves") // => 21
//iscore("chess", "swiss") // => 20

// (5)
def lowest(secrets: List[String], word: String, current: Int, acc: List[String]) : List[String] = ???

def evil(secrets: List[String], word: String) = ???


//evil(secrets, "stent").length
//evil(secrets, "hexes").length
//evil(secrets, "horse").length
//evil(secrets, "hoise").length
//evil(secrets, "house").length

// (6)
def frequencies(secrets: List[String]) : Map[Char, Double] = ???

// (7)
def rank(frqs: Map[Char, Double], s: String) = ???

def ranked_evil(secrets: List[String], word: String) = ???


}






// This template code is subject to copyright 
// by King's College London, 2022. Do not 
// make the template code public in any shape 
// or form, and do not exchange it with other 
// students under any circumstance.

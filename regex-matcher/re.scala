// Main Part 3 about Regular Expression Matching
//==============================================

object M3 {

abstract class Rexp
case object ZERO extends Rexp
case object ONE extends Rexp
case class CHAR(c: Char) extends Rexp
case class ALTs(rs: List[Rexp]) extends Rexp  // alternatives 
case class SEQs(rs: List[Rexp]) extends Rexp  // sequences
case class STAR(r: Rexp) extends Rexp         // star


//the usual binary choice and binary sequence can be defined 
//in terms of ALTs and SEQs
def ALT(r1: Rexp, r2: Rexp) = ALTs(List(r1, r2))
def SEQ(r1: Rexp, r2: Rexp) = SEQs(List(r1, r2))


// some convenience for typing regular expressions
import scala.language.implicitConversions    
import scala.language.reflectiveCalls 

def charlist2rexp(s: List[Char]): Rexp = s match {
  case Nil => ONE
  case c::Nil => CHAR(c)
  case c::s => SEQ(CHAR(c), charlist2rexp(s))
}
implicit def string2rexp(s: String): Rexp = charlist2rexp(s.toList)

implicit def RexpOps (r: Rexp) = new {
  def | (s: Rexp) = ALT(r, s)
  def % = STAR(r)
  def ~ (s: Rexp) = SEQ(r, s)
}

implicit def stringOps (s: String) = new {
  def | (r: Rexp) = ALT(s, r)
  def | (r: String) = ALT(s, r)
  def % = STAR(s)
  def ~ (r: Rexp) = SEQ(s, r)
  def ~ (r: String) = SEQ(s, r)
}

// examples for the implicits:
// ALT(CHAR('a'), CHAR('b'))
// val areg : Rexp = "a" | "b"

// SEQ(CHAR('a'), CHAR('b')) 
// val sreg : Rexp = "a" ~ "b"

// (1)
def nullable (r: Rexp) : Boolean = r match {
	case ZERO => false
	case ONE => true
	case CHAR(c) => false
	case ALTs(rs) => rs match {
		case Nil => false
		case x::xs if nullable(x) => true
		case x::xs => nullable(ALTs(xs))
	}
	case SEQs(rs) => rs match {
		case Nil => true
		case x::xs if nullable(x) => nullable(SEQs(xs))
		case _ => false
	}
	case STAR(r) => true
}

// (2) 
def der (c: Char, r: Rexp) : Rexp = r match {
	case ZERO => ZERO
	case ONE => ZERO
	case CHAR(d) => if (c == d) ONE else ZERO
	case ALTs(rs) => ALTs(rs.map(x => der(c, x)))
	case SEQs(rs) => rs match {
		case Nil => ZERO
		case x::xs if nullable(x) => ALT(SEQs(der(c, x) :: xs), der(c, SEQs(xs)))
		case x::xs => SEQs(der(c, x) :: xs)
	}
	case STAR(r) => SEQ(der(c, r), STAR(r))
}

// (3) 
def denest(rs: List[Rexp]) : List[Rexp] = rs match {
	case Nil => Nil
	case ZERO::rest => denest(rest)
	case ALTs(xs)::rest => xs ++ denest(rest)
	case x::rest => x::denest(rest)
}

// (4)
def flts(rs: List[Rexp], acc: List[Rexp] = Nil) : List[Rexp] = rs match {
	case Nil => acc
	case ZERO::rest => List(ZERO)
	case ONE::rest => flts(rest, acc)
	case SEQs(xs)::rest => flts(rest, acc:::xs)
	case r::rest => flts(rest, acc:::List(r))
}

// (5)
def ALTs_smart(rs: List[Rexp]) : Rexp = rs match {
	case Nil => ZERO
	case r::Nil => r
	case _ => ALTs(rs)
}

def SEQs_smart(rs: List[Rexp]) : Rexp = rs match {
	case Nil => ONE
	case ZERO::Nil => ZERO
	case r::Nil => r
	case _ => SEQs(rs)
}

// (6)
def simp(r: Rexp) : Rexp = r match {
	case ALTs(rs) => ALTs_smart(denest(rs.map(x => simp(x))).distinct)
	case SEQs(rs) => SEQs_smart(flts(rs.map(x => simp(x))))
	case _ => r
}

// (7)
def ders (s: List[Char], r: Rexp) : Rexp = s match {
	case Nil => r
	case c::cs => ders(cs, simp(der(c, r)))
}

def matcher(r: Rexp, s: String): Boolean = nullable(ders(s.toList, r))

// (8) 
def size(r: Rexp): Int = r match {
	case ZERO => 1
	case ONE => 1
	case CHAR(c) => 1
	case ALTs(rs) => rs match {
		case Nil => 1
		case x::xs => size(x) + size(ALTs(xs))
	}
	case SEQs(rs) => rs match {
		case Nil => 1
		case x::xs => size(x) + size(SEQs(xs))
	}
	case STAR(r) => 1 + size(r)
}


// Some testing data
//===================
/*

simp(ALT(ONE | CHAR('a'), CHAR('a') | ONE))   // => ALTs(List(ONE, CHAR(a)))
simp(((CHAR('a') | ZERO) ~ ONE) | 
     (((ONE | CHAR('b')) | CHAR('c')) ~ (CHAR('d') ~ ZERO)))   // => CHAR(a)

matcher(("a" ~ "b") ~ "c", "ab")   // => false
matcher(("a" ~ "b") ~ "c", "abc")  // => true


// the supposedly 'evil' regular expression (a*)* b
val EVIL = SEQ(STAR(STAR(CHAR('a'))), CHAR('b'))

matcher(EVIL, "a" * 1000)          // => false
matcher(EVIL, "a" * 1000 ++ "b")   // => true


// size without simplifications
size(der('a', der('a', EVIL)))             // => 36
size(der('a', der('a', der('a', EVIL))))   // => 83

// size with simplification
size(simp(der('a', der('a', EVIL))))           // => 7
size(simp(der('a', der('a', der('a', EVIL))))) // => 7

// Python needs around 30 seconds for matching 28 a's with EVIL. 
// Java 9 and later increase this to an "astonishing" 40000 a's in
// 30 seconds.
//
// Lets see how long it really takes to match strings with 
// 5 Million a's...it should be in the range of a few
// of seconds.

def time_needed[T](i: Int, code: => T) = {
  val start = System.nanoTime()
  for (j <- 1 to i) code
  val end = System.nanoTime()
  "%.5f".format((end - start)/(i * 1.0e9))
}

for (i <- 0 to 5000000 by 500000) {
  println(s"$i ${time_needed(2, matcher(EVIL, "a" * i))} secs.") 
}

// another "power" test case 
simp(Iterator.iterate(ONE:Rexp)(r => SEQ(r, ONE | ONE)).drop(50).next()) == ONE

// the Iterator produces the rexp
//
//      SEQ(SEQ(SEQ(..., ONE | ONE) , ONE | ONE), ONE | ONE)
//
//    where SEQ is nested 50 times.

*/

}

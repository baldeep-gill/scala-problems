// Shunting Yard Algorithm 
// including Associativity for Operators 
// =====================================

object C3b {


// type of tokens
type Toks = List[String]

// helper function for splitting strings into tokens
def split(s: String) : Toks = s.split(" ").toList

// left- and right-associativity
abstract class Assoc
case object LA extends Assoc
case object RA extends Assoc


// power is right-associative,
// everything else is left-associative
def assoc(s: String) : Assoc = s match {
  case "^" => RA
  case _ => LA
}


// the precedences of the operators
val precs = Map(
	"+" -> 1,
  	"-" -> 1,
	"*" -> 2,
	"/" -> 2,
    "^" -> 4
)

// the operations in the basic version of the algorithm
val ops = List("+", "-", "*", "/", "^")


def is_op(op: String) : Boolean = {
	ops.contains(op)
}

def prec(op1: String, op2: String) : Boolean = (assoc(op1), assoc(op2)) match {
	case (RA, RA) => false
	case (RA, LA) => true
	case (LA, RA) => false
	case (LA, LA) => precs.getOrElse(op1, 0) >= precs.getOrElse(op2, 0)
}

// (3) 
def syard(toks: Toks, st: Toks = Nil, out: Toks = Nil) : Toks = toks match {
	case Nil if st.isEmpty
		=> out
	case Nil
		=> syard(Nil, st.tail, out ++ List(st.head))
	case head :: tail if head.forall(_.isDigit) 
		=> syard(tail, st, out ++ List(head))
	case head :: tail if is_op(head) && (st.isEmpty || !prec(st(0), head))	// if the stack is empty OR if input operator is of higher precedence
		=> syard(tail, head::st, out)
	case head :: tail if is_op(head) //&& prec(st(0), head) // previous statement should catch other cases
		=> syard(tail, head::st.tail, out ++ List(st(0)))
	case head :: tail if head == "("
		=> syard(tail, head::st, out)
	case head :: tail if head == ")" && st.head != "("
		=> syard(toks, st.tail, out ++ List(st.head))
	case head :: tail if head == ")"
		=> syard(tail, st.tail, out)
}


// test cases
// syard(split("3 + 4 * 8 / ( 5 - 1 ) ^ 2 ^ 3"))  // 3 4 8 * 5 1 - 2 3 ^ ^ / +


// (4)
def compute(toks: Toks, stack: List[Int] = Nil) : Int = toks match {
	case Nil if stack.isEmpty
		=> 0
	case Nil
		=> stack.head
	case head :: tail if head == "^"
		=> compute(tail, (BigInt(stack.tail.head).pow(stack.head).toInt)::stack.tail.tail)
	case head::tail if head == "+"
		=> compute(tail, (stack.tail.head + stack.head)::stack.tail.tail)
	case head::tail if head == "-"
		=> compute(tail, (stack.tail.head - stack.head)::stack.tail.tail)
	case head::tail if head == "*"
		=> compute(tail, (stack.tail.head * stack.head)::stack.tail.tail)
	case head::tail if head == "/"
		=> compute(tail, (stack.tail.head / stack.head)::stack.tail.tail)
	case head::tail if head.forall(_.isDigit)
		=> compute(tail, head.toInt::stack)
}


// test cases
// compute(syard(split("3 + 4 * ( 2 - 1 )")))   // 7
// compute(syard(split("10 + 12 * 33")))       // 406
// compute(syard(split("( 5 + 7 ) * 2")))      // 24
// compute(syard(split("5 + 7 / 2")))          // 8
// compute(syard(split("5 * 7 / 2")))          // 17
// compute(syard(split("9 + 24 / ( 7 - 3 )"))) // 15
// compute(syard(split("4 ^ 3 ^ 2")))      // 262144
// compute(syard(split("4 ^ ( 3 ^ 2 )")))  // 262144
// compute(syard(split("( 4 ^ 3 ) ^ 2")))  // 4096
// compute(syard(split("( 3 + 1 ) ^ 2 ^ 3")))   // 65536

}

// Shunting Yard Algorithm
// by Edsger Dijkstra
// ========================

object C3a {

// type of tokens
type Toks = List[String]

// the operations in the basic version of the algorithm
val ops = List("+", "-", "*", "/")

// the precedences of the operators
val precs = Map(
	"+" -> 1,
	"-" -> 1,
	"*" -> 2,
	"/" -> 2
)

// helper function for splitting strings into tokens
def split(s: String) : Toks = s.split(" ").toList

// ADD YOUR CODE BELOW
//======================


// (1) 
def is_op(op: String) : Boolean = {
	ops.contains(op)
}

// returns true if op1 is of the same or higher precedence  
def prec(op1: String, op2: String) : Boolean = precs.getOrElse(op1, 0) >= precs.getOrElse(op2, 0)

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
//syard(split("3 + 4 * ( 2 - 1 )"))  // 3 4 2 1 - * +
//syard(split("10 + 12 * 33"))       // 10 12 33 * +
//syard(split("( 5 + 7 ) * 2"))      // 5 7 + 2 *
//syard(split("5 + 7 / 2"))          // 5 7 2 / +
//syard(split("5 * 7 / 2"))          // 5 7 * 2 /
//syard(split("9 + 24 / ( 7 - 3 )")) // 9 24 7 3 - / +

//syard(split("3 + 4 + 5"))           // 3 4 + 5 +
//syard(split("( ( 3 + 4 ) + 5 )"))    // 3 4 + 5 +
//syard(split("( 3 + ( 4 + 5 ) )"))    // 3 4 5 + +
//syard(split("( ( ( 3 ) ) + ( ( 4 + ( 5 ) ) ) )")) // 3 4 5 + +

 
// (2) 
def compute(toks: Toks, stack: List[Int] = Nil) : Int = toks match {
	case Nil if stack.isEmpty
		=> 0
	case Nil
		=> stack.head
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
// compute(syard(split("3 + 4 * ( 2 - 1 )")))  // 7
// compute(syard(split("10 + 12 * 33")))       // 406
// compute(syard(split("( 5 + 7 ) * 2")))      // 24
// compute(syard(split("5 + 7 / 2")))          // 8
// compute(syard(split("5 * 7 / 2")))          // 17
// compute(syard(split("9 + 24 / ( 7 - 3 )"))) // 15

}



// This template code is subject to copyright 
// by King's College London, 2022. Do not 
// make the template code public in any shape 
// or form, and do not exchange it with other 
// students under any circumstance.


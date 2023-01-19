// Main Part 5 about a "Compiler" for the Brainf*** language
//============================================================


object M5b {

// !!! Copy any function you need from file bf.scala !!!
//
// If you need any auxiliary function, feel free to 
// implement it, but do not make any changes to the
// templates below.


// DEBUGGING INFORMATION FOR COMPILERS!!!
//
// Compiler, even real ones, are fiendishly difficult to get
// to produce correct code. One way to debug them is to run
// example programs ``unoptimised''; and then optimised. Does
// the optimised version still produce the same result?


// for timing purposes
def time_needed[T](n: Int, code: => T) = {
	val start = System.nanoTime()
	for (i <- 0 until n) code
	val end = System.nanoTime()
	(end - start)/(n * 1.0e9)
}


type Mem = Map[Int, Int]

import io.Source
import scala.util._

// ADD YOUR CODE BELOW
//======================
//

def compute(prog: String, pc: Int, mp: Int, mem: Mem) : Mem = {
    if (pc == prog.length) mem
    else {
        prog(pc) match {
            case '>' => compute(prog, pc + 1, mp + 1, mem)
            case '<' => compute(prog, pc + 1, mp - 1, mem)
            case '+' => compute(prog, pc + 1, mp, write(mem, mp, sread(mem, mp) + 1))
            case '-' => compute(prog, pc + 1, mp, write(mem, mp, sread(mem, mp) - 1))
            case '.' => {
                print(sread(mem, mp).toChar)
                compute(prog, pc + 1, mp, mem)
            }
            case '[' if sread(mem, mp) == 0 => compute(prog, jumpRight(prog, pc + 1, 0), mp, mem)
            case '[' => compute(prog, pc + 1, mp, mem)
            case ']' if sread(mem, mp) != 0 => compute(prog, jumpLeft(prog, pc - 1, 0), mp, mem)
            case ']' => compute(prog, pc + 1, mp, mem)
            case x => compute(prog, pc + 1, mp, mem)
        }
    }
}

def run(prog: String, m: Mem = Map()) : Mem = compute(prog, 0, 0, m)

def load_bff(name: String) : String = Try(Source.fromFile(name).mkString).getOrElse("")

def sread(mem: Mem, mp: Int) : Int = mem.getOrElse(mp, 0)

def write(mem: Mem, mp: Int, v: Int) : Mem = mem + (mp -> v)

def jumpRight(prog: String, pc: Int, level: Int) : Int = {
    if (pc == prog.length) pc
    else {
        prog(pc) match {
            case '[' => jumpRight(prog, pc + 1, level + 1)
            case ']' if level > 0 => jumpRight(prog, pc + 1, level - 1)
            case ']' => pc + 1
            case x => jumpRight(prog, pc + 1, level)
        }
    }
}

def jumpLeft(prog: String, pc: Int, level: Int) : Int = {
    if (pc == -1) pc
    else {
        prog(pc) match {
            case ']' => jumpLeft(prog, pc - 1, level + 1)
            case '[' if level > 0 => jumpLeft(prog, pc - 1, level - 1)
            case '[' => pc + 1
            case x => jumpLeft(prog, pc - 1, level)
        }
    }
}

// (6) 
def jtable(pg: String) : Map[Int, Int] = {
	def helper(pg:String, index: Int, table: Map[Int, Int]) : Map[Int, Int] = {
		if (index == pg.length) table
		else {
			pg(index) match {
				case '[' => helper(pg, index + 1, table + (index -> jumpRight(pg, index + 1, 0)))
				case ']' => helper(pg, index + 1, table + (index -> jumpLeft(pg, index - 1, 0)))
				case x => helper(pg, index + 1, table)
			}
		}
	}
	helper(pg, 0, Map[Int, Int]())
}

// testcase
//
// jtable("""+++++[->++++++++++<]>--<+++[->>++++++++++<<]>>++<<----------[+>.>.<+<]""")
// =>  Map(69 -> 61, 5 -> 20, 60 -> 70, 27 -> 44, 43 -> 28, 19 -> 6)


def compute2(pg: String, tb: Map[Int, Int], pc: Int, mp: Int, mem: Mem) : Mem = {
	if (pc == pg.length) mem
    else {
        pg(pc) match {
            case '>' => compute2(pg, tb, pc + 1, mp + 1, mem)
            case '<' => compute2(pg, tb, pc + 1, mp - 1, mem)
            case '+' => compute2(pg, tb, pc + 1, mp, write(mem, mp, sread(mem, mp) + 1))
            case '-' => compute2(pg, tb, pc + 1, mp, write(mem, mp, sread(mem, mp) - 1))
            case '.' => {
                print(sread(mem, mp).toChar)
                compute2(pg, tb, pc + 1, mp, mem)
            }
            case '[' if sread(mem, mp) == 0 => compute2(pg, tb, sread(tb, pc), mp, mem)
            case '[' => compute2(pg, tb, pc + 1, mp, mem)
            case ']' if sread(mem, mp) != 0 => compute2(pg, tb, sread(tb, pc), mp, mem)
            case ']' => compute2(pg, tb, pc + 1, mp, mem)
            case x => compute2(pg, tb, pc + 1, mp, mem)
        }
    }
}

def run2(pg: String, m: Mem = Map()) = compute2(pg, jtable(pg), 0, 0, m)

// testcases
// time_needed(1, run(load_bff("benchmark.bf")))
// run1 = 19.941300937
// time_needed(1, run2(load_bff("benchmark.bf")))
// run2 = 21.951717765

// time_needed(1, run(load_bff("sierpinski.bf")))
// run1 = 0.074708574
// time_needed(1, run2(load_bff("sierpinski.bf")))
// run2 = 0.099770702

// (7) 

def optimise(s: String) : String = s.replaceAll("""[^<>+\-.\[\]]""", "").replaceAll("""\[-\]""", "0")

def compute3(pg: String, tb: Map[Int, Int], pc: Int, mp: Int, mem: Mem) : Mem = {
	if (pc == pg.length) mem
    else {
        pg(pc) match {
            case '>' => compute3(pg, tb, pc + 1, mp + 1, mem)
            case '<' => compute3(pg, tb, pc + 1, mp - 1, mem)
            case '+' => compute3(pg, tb, pc + 1, mp, write(mem, mp, sread(mem, mp) + 1))
            case '-' => compute3(pg, tb, pc + 1, mp, write(mem, mp, sread(mem, mp) - 1))
            case '.' => {
                print(sread(mem, mp).toChar)
                compute3(pg, tb, pc + 1, mp, mem)
            }
            case '[' if sread(mem, mp) == 0 => compute3(pg, tb, sread(tb, pc), mp, mem)
            case '[' => compute3(pg, tb, pc + 1, mp, mem)
            case ']' if sread(mem, mp) != 0 => compute3(pg, tb, sread(tb, pc), mp, mem)
            case ']' => compute3(pg, tb, pc + 1, mp, mem)
			case '0' => compute3(pg, tb, pc + 1, mp, write(mem, mp, 0))
            case x => compute3(pg, tb, pc + 1, mp, mem)
        }
    }
}

def run3(pg: String, m: Mem = Map()) = compute3(pg, jtable(pg), 0, 0, m)


// testcases
//
// optimise(load_bff("benchmark.bf"))          // should have inserted 0's
// optimise(load_bff("mandelbrot.bf")).length  // => 11203
// 
// time_needed(1, run3(optimise(load_bff("benchmark.bf"))))
// run3 = 11.349253759

// (8)
// A is 65 in ASCII - maybe make offset 'length + 64'
// + - < >
def combine(s: String) : String = {
	def helper(s: String, token: Char, length: Int, processed: String = "") : String = s.toList match {
		match Nil => processed

	}
}

// testcase
// combine(load_bff("benchmark.bf"))

def compute4(pg: String, tb: Map[Int, Int], pc: Int, mp: Int, mem: Mem) : Mem = ???

// should call first optimise and then combine on the input string
//
def run4(pg: String, m: Mem = Map()) = ???


// testcases
// combine(optimise(load_bff("benchmark.bf"))) // => """>A+B[<A+M>A-A]<A[[....."""

// testcases (they should now run much faster)
// time_needed(1, run4(load_bff("benchmark.bf")))
// time_needed(1, run4(load_bff("sierpinski.bf"))) 
// time_needed(1, run4(load_bff("mandelbrot.bf")))


}





// This template code is subject to copyright 
// by King's College London, 2022. Do not 
// make the template code public in any shape 
// or form, and do not exchange it with other 
// students under any circumstance.

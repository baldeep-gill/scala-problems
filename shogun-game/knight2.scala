// Core Part about finding a single tour for a board using the
// Warnsdorf Rule
//==============================================================

object M4b {

// !!! Copy any function you need from file knight1.scala !!!
//
// If you need any auxiliary functions, feel free to 
// implement them, but do not make any changes to the
// templates below.

type Pos = (Int, Int)    // a position on a chessboard 
type Path = List[Pos]    // a path...a list of positions

// ADD YOUR CODE BELOW
//======================

//(6) 
def ordered_moves(dim: Int, path: Path, x: Pos) : List[Pos] = ???


//(7) 
def first_closed_tour_heuristics(dim: Int, path: Path) : Option[Path] = ???


//(8) Same as (7) but searches for *non-closed* tours. This 
//    version of the function will be called with dimensions of 
//    up to 30 * 30.

def first_tour_heuristics(dim: Int, path: Path) : Option[Path] = ???



}





// This template code is subject to copyright 
// by King's College London, 2022. Do not 
// make the template code public in any shape 
// or form, and do not exchange it with other 
// students under any circumstance.

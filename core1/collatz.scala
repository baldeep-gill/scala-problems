// Core Part 1 about the 3n+1 conjecture
//============================================

object C1 {

// ADD YOUR CODE BELOW
//======================


//(1) 
def collatz(n: Long) : Long = {
    if (n == 1) 0
    else if (n % 2 == 0) 1 + collatz(n / 2)
    else 1 + collatz(3 * n + 1)
}

//(2) 
def collatz_max(bnd: Long) : (Long, Long) = {
    val step_list = (1L to bnd).toList.map(n => collatz(n))
    val max_steps = step_list.max
    val input_number = step_list.indexOf(max_steps) + 1
    (max_steps, input_number)
}

//(3)
def is_pow_of_two(n: Long) : Boolean = {
    if (n == 0) false
    else if (n & (n - 1) == 0) true
    else false
}

def is_hard(n: Long) : Boolean = is_pow_of_two(3 * n + 1)

def last_odd(n: Long) : Long = {
    // last odd number in the series will be if is_hard is true
}

}



// This template code is subject to copyright 
// by King's College London, 2022. Do not 
// make the template code public in any shape 
// or form, and do not exchange it with other 
// students under any circumstance.

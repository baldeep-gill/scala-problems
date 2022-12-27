// Main Part 1 about a really dumb investment strategy
//===================================================

object M1 {

//two test portfolios

val blchip_portfolio = List("GOOG", "AAPL", "MSFT", "IBM", "FB", "AMZN", "BIDU")
val rstate_portfolio = List("PLD", "PSA", "AMT", "AIV", "AVB", "BXP", "CCI", 
                            "DLR", "EQIX", "EQR", "ESS", "EXR", "FRT", "HCP") 

import io.Source
import scala.util._

// ADD YOUR CODE BELOW
//======================


// (1) 
def get_january_data(symbol: String, year: Int) : List[String] = {
    Try(Source.fromFile(s"${symbol}.csv")("ISO-8859-1").getLines().toList).getOrElse(List()).filter(_.startsWith(s"${year}"))
}


// (2) 
def get_first_price(symbol: String, year: Int) : Option[Double] = {
    Try(Some(get_january_data(symbol, year).head.split(",").toList(1).toDouble)).getOrElse(None)
}


// (3) 
def get_prices(portfolio: List[String], years: Range) : List[List[Option[Double]]] = {
    for (n <- years.toList) yield {
        for (m <- portfolio) yield {
            get_first_price(m, n)
        }
    }
}


// (4) 
def get_delta(price_old: Option[Double], price_new: Option[Double]) : Option[Double] = {
    Try(Some((price_new.get - price_old.get)/price_old.get)).getOrElse(None)
}


// (5)
def get_deltas(data: List[List[Option[Double]]]) :  List[List[Option[Double]]] = {
    for (n <- data.sliding(2, 1).toList) yield {
        for (m <- n.transpose) yield {
            get_delta(m(0), m(1))
        }
    }
}

// (6) 
def yearly_yield(data: List[List[Option[Double]]], balance: Long, index: Int) : Long = {
    val deltas = data(index).flatten
    balance + sum_profits(deltas, balance/deltas.length).toLong
}

def sum_profits(deltas: List[Double], split_balance: Long) : Double = {
    if (deltas.isEmpty) 0D 
    else (deltas.head * split_balance) + sum_profits(deltas.tail, split_balance)
}


// (7) 
def compound_yield(data: List[List[Option[Double]]], balance: Long) : Long = {
    if (data.isEmpty) balance
    else compound_yield(data.tail, yearly_yield(data, balance, 0))
}

def investment(portfolio: List[String], years: Range, start_balance: Long) : Long = {
    val raw_deltas = get_deltas(get_prices(portfolio, years))
    start_balance + compound_yield(raw_deltas, start_balance)
}




//Test cases for the two portfolios given above

// println("Real data: " + investment(rstate_portfolio, 1978 to 2019, 100))
// println("Blue data: " + investment(blchip_portfolio, 1978 to 2019, 100))


}




// This template code is subject to copyright 
// by King's College London, 2022. Do not 
// make the template code public in any shape 
// or form, and do not exchange it with other 
// students under any circumstance.

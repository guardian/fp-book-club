package free.monoid

import cats.Monoid

/*
 * Free Monoid - uses the custom one in FreeMonoid.scala
 */

//*******************************
// Algebra
//*******************************
object Algebra {

  enum LettersA:
    case J, O, H, N

  import LettersA.*
  // convenience methods
  def j: Letters =
    Suspend(J)
  def o: Letters =
    Suspend(O)
  def h: Letters =
    Suspend(H)
  def n: Letters =
    Suspend(N)

}

import free.monoid.Algebra.*
import free.monoid.Algebra.LettersA.*

type Letters = FreeMonoid[LettersA]


//*******************************
// interpreter (for String)
//*******************************
object Interpreter {
  def interpret: LettersA => String = {
    case J => "j"
    case O => "o"
    case H => "h"
    case N => "n"
  }
}


//*******************************
// program
//*******************************
def program(): (Letters, Letters) = {
  val result = n.combineWith(o)

  val result2 = j.combineWith(o).combineWith(h).combineWith(n)

  (result, result2)
}

//*******************************
// main
//*******************************
object Main extends App {

    val ex = program()
    val (result, result2) = ex
    val r = result.foldMap(Interpreter.interpret)
    val r2 = result2.foldMap(Interpreter.interpret)
    println("result: " + r)
    println("result2: " + r2)
}




// this is built in to monads, implemented as flatMap.
given Monoid[String] = new Monoid[String] {
  override def empty: String = ""
  override def combine(x: String, y: String): String = x + y
}

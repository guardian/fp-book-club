package tagless.monoid

import cats.Monoid

/*
 * Free Monoid finally tagless
 */

//*******************************
// Algebra
//*******************************
trait Letters[A] {

  def j: A
  def o: A
  def h: A
  def n: A

}


//*******************************
// interpreter (for String)
//*******************************
given Letters[String] = new Letters[String] {
  override def j: String = "j"

  override def o: String = "o"

  override def h: String = "h"

  override def n: String = "n"
}


//*******************************
// program
//*******************************
def program[A: Monoid]()(using letters: Letters[A]): (A, A) = {
  val combine = summon[Monoid[A]].combine _
  val result = combine(letters.n, letters.o)

  val result2 = combine(
    combine(letters.j, letters.o),
    combine(letters.h, letters.n)
  )

  (result, result2)
}

//*******************************
// main
//*******************************
object Main extends App {

    val ex = program[String]()
    val (result, result2) = ex
    println("result: " + result)
    println("result2: " + result2)
}




// this is built in to monads, implemented as flatMap.
given Monoid[String] = new Monoid[String] {
  override def empty: String = ""
  override def combine(x: String, y: String): String = x + y
}

package tagless.monoid

import cats.{Functor, Monad, Monoid}
import cats.implicits.*

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}

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

given Letters[Future[String]] = new Letters[Future[String]] {
  override def j: Future[String] = Future.successful("j")

  override def o: Future[String] = Future.successful("o")

  override def h: Future[String] = Future.successful("h")

  override def n: Future[String] = Future.successful("n")
}


//*******************************
// program
//*******************************
def program[A: Monoid]()(using letters: Letters[A]): (A, A) = {
  import letters.*

  val combine = summon[Monoid[A]].combine _
  val result = combine(n, o)

  val result2 = combine(
    combine(j, o),
    combine(h, n)
  )

  (result, result2)
}

//*******************************
// main
//*******************************
object Main extends App {
  {
    val ex = program[String]()
    val (result, result2) = ex
    println("result: " + result)
    println("result2: " + result2)
  }
  {
    given ExecutionContext = ExecutionContext.global
    val ex = program[Future[String]]()
    val (result, result2) = ex
    println("result: " + Await.result(result, Duration.Inf))
    println("result2: " + Await.result(result2, Duration.Inf))
  }
}


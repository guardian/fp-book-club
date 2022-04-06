package tagless

import cats.Monad
import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits.*

import scala.concurrent.Await
import scala.concurrent.duration.Duration

/*
 * https://typelevel.org/blog/2019/02/06/algebraic-api-design.html
 */

//*******************************
// Algebra of a calculator
//*******************************
trait Calculator[F[_]] {

  def plus(a: Int, b: Int): F[Int]

}


//*******************************
// interpreter (for IO)
//*******************************
given Calculator[IO] = new Calculator[IO] {
  override def plus(a: Int, b: Int): IO[Int] =
    IO.pure(a + b)
}


//*******************************
// program
//*******************************
def program[F[_]: Monad]()(using calculator: Calculator[F]): (F[Int], F[Int]) = {

  val result = for {
    program <- calculator.plus(1, 4)
    aaa <- calculator.plus(program, 30)
    result <- calculator.plus(aaa, 4)
  } yield result


  // for comprehension is giving us the ability
  // to look at the integer itself
  val result2 = for {
    p <- calculator.plus(1, 4)
    x = if (p % 2 == 1) 10 * p else 0
    z <- calculator.plus(x, 4)
  } yield z

  (result, result2)
}


//*******************************
// main
//*******************************
object Main extends IOApp {

  override def run(args: List[String]): IO[ExitCode] = {
    val ex: (IO[Int], IO[Int]) = program[IO]()
    val (result, result2) = ex
    for {
      r <- result
      r2 <- result2
    } yield {
      println("result: " + r)
      println("result2: " + r2)
      ExitCode.Success
    }
  }
}




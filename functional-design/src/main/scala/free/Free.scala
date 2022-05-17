package free

import cats.free.Free
import cats.arrow.FunctionK
import cats.effect.{ExitCode, IO, IOApp}
import cats.{Id, ~>}

/*
 * https://typelevel.org/cats/datatypes/freemonad.html
 */

//*******************************
// Algebra of a calculator
//*******************************
object Algebra {

  sealed trait CalculatorA[A]

  case class Plus(num1: Int, num2: Int) extends CalculatorA[Int]

  // convenience method
  def plus(num1: Int, num2: Int): Calculator =
    Free.liftF[CalculatorA, Int](Plus(num1, num2))

}

import Algebra._

type Calculator = Free[CalculatorA, Int]


//*******************************
// interpreter (for IO)
//*******************************
object Interpreter {
  def interpret: CalculatorA ~> IO = new (CalculatorA ~> IO) {
    override def apply[A](calc: CalculatorA[A]): IO[A] =
      calc match {
        case Plus(a1, a2) => IO.pure(a1 + a2)
      }
  }
}


//*******************************
// program
//*******************************
def program() = {
  val result = for {
    program <- plus(1, 4)
    aaa <- plus(program, 30)
    result <- plus(aaa, 4)
  } yield result


  val result2 = for {
    p <- plus(1, 4)
    x = if (p % 2 == 1) 10 * p else 0
    z <- plus(x, 4)
  } yield z

  (result, result2)
}


//*******************************
// main
//*******************************
object Main extends IOApp {

  override def run(args: List[String]): IO[ExitCode] = {
    val ex = program()
    val (result, result2) = ex
    for {
      r <- result.foldMap(Interpreter.interpret)
      r2 <- result2.foldMap(Interpreter.interpret)
    } yield {
      println("result: " + r)
      println("result2: " + r2)
      ExitCode.Success
    }
  }
}




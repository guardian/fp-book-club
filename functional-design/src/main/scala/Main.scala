
// Algebra of a calculator
object Algebra {
  sealed trait Calculator {
    def flatMap(a: Int => Calculator): Calculator = FlatMap(this, a)
    def map(a: Int => Int): Calculator = Map(this, a)
  }

  case class FlatMap(self: Calculator, a: Int => Calculator) extends Calculator
  case class Map(self: Calculator, a: Int => Int) extends Calculator

  case class Number(value: Int) extends Calculator

  case class Plus(num1: Calculator, num2: Calculator) extends Calculator

}

import Algebra._

object Interpreter {
  def interpret (calc: Calculator): Int = calc match {
    case Number(a) => a
    case Plus(a1, a2) => interpret(a1) + interpret(a2)
    case FlatMap(self, a) => interpret(a(interpret(self)))
    case Map(self, a) => a(interpret(self))
  }
}

object Main extends App {
  val num1 = Number(1)
  val num2 = Number(4)

  val program = Plus(num1, num2)

  val result = Plus(Plus(program, Number(30)), Number(4))

  // for comprehension is giving us the ability
  // to look at the integer itself
  val result2 = for {
    p <- program
    x <- if (p % 2 == 1) Number(10 * p) else Number(0)
    z <- Plus(Number(x), Number(4))
  } yield (z)

  println(result)

  println(Interpreter.interpret(result))
}




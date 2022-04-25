package free

import free.Algebra.{GetObject, GetObject2, ProcessData, ProcessData2, PutObject, PutObject2, S3}

//*******************************
// Algebra of a S3
//*******************************
object Algebra {
  sealed trait S3[RETURN]
  case class GetObject(name: String) extends S3[String]
  case class PutObject(name: String, input: S3[String]) extends S3[Unit]
  case class ProcessData(f: String => String, input: S3[String]) extends S3[String]


  sealed trait S32[INPUT]
  case class GetObject2(name: String, next: S32[String]) extends S32[Unit]
  case class PutObject2(name: String) extends S32[String]
  case class ProcessData2(f: String => String, next: S32[String]) extends S32[String]
}

//*******************************
// program
//*******************************
def program() = {

  // we need to get an object, make it upper case, then put it
  val program = PutObject("output.txt", ProcessData(_.toUpperCase, GetObject("source.txt")))

  val program2 =
    GetObject2("source.txt",
      ProcessData2(_.toUpperCase,
        PutObject2("output.txt")
      )
    )

}

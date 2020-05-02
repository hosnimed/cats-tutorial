

object Validated extends App {

  import cats.data.Validated
  import cats.syntax.either._

  import scala.util.Try

  type FormData = Map[String, String]
  type FailFast[A] = Either[List[String], A]
  type FailSlow[A] = Validated[List[String], A]

  def getValue(field: String)(formData: FormData):FailFast[String] = formData.get(field).toRight(List(s"Field : $field not found."))
  def parseInt(field: String)(value: String):FailFast[Int] = Either.fromTry(Try(value.toInt)).leftMap(_ => List(s"$field value must be an integer"))

  def notBlank(name: String)(value: String):FailFast[String] = Right(value).ensure(List(s"$name cannot be blank"))(_.nonEmpty)
  def nonNegative(name: String)(value: Int):FailFast[Int] = Right(value).ensure(List(s"$name cannot be negative"))(_ >= 0)

  def readName(data: FormData):FailFast[String] = getValue("name")(data) flatMap notBlank("name")
  def readAge(data: FormData):FailFast[Int] = getValue("age")(data) flatMap notBlank("age") flatMap parseInt("age") flatMap nonNegative("age")

  case class User(name: String, age: Int)

  def readUser(data: FormData): FailSlow[User] = {
    import cats.instances.list._
    import cats.syntax.apply._ // for mapN

//    Semigroupal[FailSlow].product[String, Int]
    (
      readName(data).toValidated,
      readAge(data).toValidated
      )
//      .mapN((name, age) => User(name, age))
      .mapN(User.apply)
  }

  println(readUser(Map("name" -> "john", "age" -> "30")))
  println(readUser(Map("name" -> "", "age" -> "30")))
  println(readUser(Map("name" -> "john", "age" -> "")))
  println(readUser(Map("name" -> "john", "age" -> "nan")))
  println(readUser(Map("name" -> "", "age" -> "")))
  println(readUser(Map("name" -> "", "age" -> "nan")))
}

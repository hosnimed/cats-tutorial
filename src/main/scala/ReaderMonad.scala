object ReaderMonad extends App {

  import cats.data.Reader

  case class Cat(name:String, food:String)
  val harry = Cat("Harry", "Donuts")

  val catName: Reader[Cat, String] = Reader(cat => cat.name)
  catName.run(harry)
  val greeting: Reader[Cat, String] = catName.map(name => s"Hello $name")
  greeting.run(harry)
  val feeding: Reader[Cat, String] = Reader(cat => s"Nice bowl of ${cat.food}")
  feeding.run(harry)

  val compose = for{
    greet <- greeting
    feed  <- feeding
  } yield greet +", "+ feed
  compose.run(harry)

  /********exercise*****/
  import cats.syntax.applicative._
  case class Db(
                 usernames: Map[Int, String],
                 passwords: Map[String, String]
               )

  type DbReader[A]=Reader[Db, A]
  def findUsername(userId: Int): DbReader[Option[String]] = Reader(db => db.usernames.get(userId))
  def checkPassword(username: String, password: String): DbReader[Boolean] = Reader(db => db.passwords.get(username).contains(password))
  def checkLogin(userId: Int, password: String): DbReader[Boolean] = for {
    mayBeUsername <- findUsername(userId)
    isPasswordMatch <- mayBeUsername.map { username => checkPassword(username, password) }.getOrElse {false.pure[DbReader]}
  } yield isPasswordMatch

  val users = Map( 1 -> "dade", 2 -> "kate", 3 -> "margo")
  val passwords = Map( "dade" -> "zerocool", "kate" -> "acidburn", "margo" -> "secret")
  val db = Db(users, passwords)
  println(checkLogin(1, "zerocool").run(db))
  println(checkLogin(2, "acid").run(db))
}

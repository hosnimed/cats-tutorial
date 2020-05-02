import cats.data.Writer

object MonadTransformer extends App {

  /**Either of Option*/
  import cats.data.OptionT

  type ErrorOr[A] = Either[String, A]
  type ErrorOrOption[A] = OptionT[ErrorOr, A]

  import cats.instances.either._
  import cats.syntax.applicative._

  val a: ErrorOrOption[Int] = 10.pure[ErrorOrOption]
  val b: ErrorOrOption[Int] = 5.pure[ErrorOrOption]
  println(a.value)

  /**Future of Either of Option*/
  import cats.data.EitherT

  import scala.concurrent.ExecutionContext.Implicits.global
  import scala.concurrent.{Await, Future}
  import scala.concurrent.duration._

  type FutureEither[A] = EitherT[Future, String, A]
  type FutureEitherOption[A] = OptionT[FutureEither, A]

  import cats.instances.future._
  val value: Either[String, Option[Int]] = Await.result(10.pure[FutureEitherOption].value.value, 1.seconds)
  println(value)

  /***/
    type Logged[A] = Writer[List[String], A]
  // Methods generally return untransformed stacks:
  def parseNumber(str: String): Logged[Option[Int]] = util.Try(str.toInt).toOption match {
    case Some(num) => Writer(List(s"Read $str"), Some(num))
    case None      => Writer(List(s"Failed on $str"), None)
  }
  // Consumers use monad transformers locally to simplify composition:
  def addAll(a: String, b: String, c: String): Logged[Option[Int]] = {
    import cats.data.OptionT
    import cats.implicits._
    val result = for {
      a <- OptionT(parseNumber(a))
      b <- OptionT(parseNumber(b))
      c <- OptionT(parseNumber(c))
    } yield a + b + c
    result.value
  }
  println(addAll("10", "5a", "5"))

  /******Ex****/
  import cats.data.EitherT
  import cats.instances.future._

  import scala.concurrent.ExecutionContext.Implicits.global
  import scala.concurrent.{Await, Future}
  import scala.concurrent.duration._

  type Response[A] = EitherT[Future, String, A]
  val powerLevels = Map("Jazz" -> 6, "Bumblebee" -> 8, "Hot Rod" -> 10)

  def getPowerLevel(autobot: String): Response[Int] = {
    powerLevels.get(autobot) match {
      case Some(bot) => EitherT.right(Future(bot))
      case None => EitherT.left(Future(s"$autobot is unreachble"))
    }
  }
  def canSpecialMove(ally1: String, ally2: String): Response[Boolean] = {
    for {
      bot1 <- getPowerLevel(ally1)
      bot2 <- getPowerLevel(ally2)
    } yield bot1+bot2 > 15
  }
  def tacticalReport(ally1: String, ally2: String): String = {
    Await.result(canSpecialMove(ally1, ally2).value, 1.second) match {
      case Right(b) => if (b) s"$ally1 and $ally2 are ready to roll out!" else s"$ally1 and $ally2 need a recharge."
      case Left(a) => s"Comms error: $a"
    }
  }

  println(tacticalReport("Jazz", "Bumblebee"))
  println(tacticalReport("Bumblebee", "Hot Rod"))
  println(tacticalReport("Bee", "Ironhide"))
}
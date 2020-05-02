object WriterMonad extends App {

  def slowly[A](body: => A): A = {
    try body
    finally Thread.sleep(100)
  }
  import cats.data.Writer
  type Logged[A] = Writer[Vector[String], A]

  import cats.instances.vector._
  import cats.syntax.applicative._
//  import cats.implicits._
  println(10.pure[Logged])

  import cats.syntax.writer._
  println(Vector("just error").tell)
  println(10.writer(Vector("maybe error")))
  println(10.pure[Logged].map(_ * 2))

  def factorial(n: Int): Logged[Int] = {
    for {
      ans <- if (n == 0){
        1.pure[Logged]
      } else {
        slowly(factorial(n-1).map(_ * n))
      }
      _ <- Vector(s"Fact $n = $ans").tell
    } yield ans
  }

  import scala.concurrent.ExecutionContext.Implicits.global
  import scala.concurrent._
  import scala.concurrent.duration._

  Await.result(Future.sequence(Vector( Future(factorial(3)), Future(factorial(3)))), 5.seconds)
    .foreach { logged =>
      val (log, fact) = logged.run
      println(s"(log, fact) = ($log, $fact)")
    }
}

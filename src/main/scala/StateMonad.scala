object StateMonad extends App {

  import cats.data.State

  final case class Robot( id: Long,
                          sentient: Boolean,
                          name: String,
                          model: String)

  final case class Seed(long: Long) {
    def next = Seed(long * 6364136223846793005L + 1442695040888963407L)
  }

  def nextLong:State[Seed, Long] = State { seed =>
    (seed.next, seed.long)
  }

  def nextBoolean:State[Seed, Boolean] = nextLong.map(_  > 0)

  def createRobot:State[Seed, Robot] = {
    val b = nextBoolean
    for {
      id <- nextLong
      sentient <- b
      isCatherine <- b
      name = if (isCatherine) "Catherine" else "Carlos"
      isReplicant <- b
      model = if (isReplicant) "replicant" else "borg"
    } yield Robot(id, sentient, name, model)
  }

  val initialSeed = Seed(10)
  val (seed, robot) = createRobot.run(initialSeed).value
  println(s"(seed, robot) = ($seed, $robot)")

  /***StateT*/

/*
  final case class AsyncSeed(long: Long) {
    def next = Future {
      AsyncSeed(long * 6364136223846793005L + 1442695040888963407L)
    }
  }
  def nextLong:StateT[Future, AsyncSeed, Long] = StateT { (seed:AsyncSeed) =>
    seed.next zip Future.successful(seed.long)
  }
  import cats.data.Writer
  type Logged[A] = Writer[Vector[String], A]
  def nextBoolean:StateT[Future, AsyncSeed, Boolean] = nextLong.map(_ > 0)
  def createRobotAsync:StateT[Future, AsyncSeed, Logged[Robot]] = {
    import cats.syntax.writer._
    val b = nextBoolean
    for {
      id <- nextLong
      sentient <- b
      isCatherine <- b
      name = if (isCatherine) "Catherine" else "Carlos"
      isReplicant <- b
      model = if (isReplicant) "replicant" else "borg"
      w = Vector(s"Robot($id, $sentient, $name, $model)")
    } yield {
      val robot = Robot(id, sentient, name, model)
      println("Robot : "+robot)
      robot.writer(w)
    }
  }

  val asyncSeed = AsyncSeed(10)
  createRobotAsync.runA(asyncSeed).value
    .map(_.map( logged => logged.written))

  /**IndexedStateT*/

  import cats.Eval
  import cats.data.IndexedStateT

  sealed trait DoorStatus
  case object Open extends DoorStatus
  case object Closed extends DoorStatus

  def open:IndexedStateT[Eval, Closed.type, Open.type, Unit] = IndexedStateT.set(Open)
  def close:IndexedStateT[Eval, Open.type, Closed.type, Unit] = IndexedStateT.set(Closed)

  /*
  val invalid = for {
    _ <- open
    _ <- close
    _ <- close
  } yield ()
  */

  val valid
  : IndexedStateT[Eval, Closed.type, Open.type , Unit]
  = for {
    _ <- open
    _ <- close
    _ <- open
  } yield ()
  valid.run(Closed).value

 */
}

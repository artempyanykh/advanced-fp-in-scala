object Day2 {

  import java.util.concurrent._

  trait Monad[F[_]] {
    def point[A](a: A): F[A]

    def bind[A, B](fa: F[A])(afb: A => F[B]): F[B]

    def fmap[A, B](ab: A => B): F[A] => F[B]
  }

  object Monad {
    def apply[F[_]](implicit F: Monad[F]): Monad[F] = F
  }

  implicit class MonadSyntax[F[_], A](fa: F[A]) {
    def map[B](f: A => B)(implicit F: Monad[F]): F[B] = F.fmap(f)(fa)

    def flatMap[B](f: A => F[B])(implicit F: Monad[F]): F[B] = F.bind(fa)(f)
  }

  implicit class PointSyntax[A](a: A) {
    def point[F[_]](implicit F: Monad[F]): F[A] = F.point(a)
  }

  final abstract class Void {
    def absurd[A]: A
  }

  final case class IO[E, A](unsafePerformIO: () => Either[E, A]) { self =>
    def map[B](f: A => B): IO[E, B] = {
      IO(() => {
        self.unsafePerformIO() match {
          case Left(e) => Left(e)
          case Right(a) => Right(f(a))
        }
      })
    }

    def flatMap[B](f: A => IO[E, B]): IO[E, B] = {
      IO(() => {
        self.unsafePerformIO() match {
          case Left(e) => Left(e)
          case Right(a) => f(a).unsafePerformIO()
        }
      })
    }

    def mapError[E2](f: E => E2): IO[E2, A] =
      IO(() => self.unsafePerformIO() match {
        case Left(e) => Left(f(e))
        case Right(a) => Right(a)
      })

    def attempt: IO[Void, Either[E, A]] = IO(() => {
      Right(self.unsafePerformIO())
    })

    def fork(implicit pool: ForkJoinPool): IO[Void, IO[E, A]] = IO(() => {
      val task = pool.submit(
        new Callable[Either[E, A]] {
          def call(): Either[E, A] = {
          self.unsafePerformIO()
          }
        }
      )

      Right(IO(() => task.join()))
    })
  }

  object IO {
    def sync[A](effect: => A): IO[Exception, A] =
      IO(() => {
        try Right(effect)
        catch { case e: Exception => Left(e) }
      })

    def point[E, A](a: A): IO[E, A] = IO(() => Right(a)) 
    def fail[E, A](e: E): IO[E, A] = IO(() => Left(e))

    def absolve[E, A](io: IO[Void, Either[E, A]]): IO[E, A] =
      IO(() => {
        io.unsafePerformIO() match {
          case Left(n) => n.absurd[Either[E, A]]
          case Right(e) => e
        }
      })

    implicit def MonadIO[E]: Monad[IO[E, ?]] = new Monad[IO[E, ?]] {
      def point[A](a: A): IO[E, A] = IO.point(a)

      def bind[A, B](fa: IO[E, A])(afb: A => IO[E, B]): IO[E, B] = fa.flatMap(afb)

      def fmap[A, B](ab: A => B): IO[E, A] => IO[E, B] = io => io.map(ab)
    }

    implicit def ConsoleIOIO: ConsoleIO[IO[Exception, ?]] = new ConsoleIO[IO[Exception, ?]] {
        def println(line: String): IO[Exception, Unit] = {
            IO.sync { 
              line foreach scala.Console.print
              scala.Console.println("")
            }
          // IO.sync(scala.Console.println(line))
        }
          

        def readln: IO[Exception, String] =
          IO.sync(scala.io.StdIn.readLine())
    }

    implicit def LoggingIO: Logging[IO[Exception, ?]] = new Logging[IO[Exception, ?]] {
      def log(level: Level)(message: => String): IO[Exception, Unit] = IO.sync {
        java.util.logging.Logger.getLogger("IO").log(
          level match {
            case Level.Info => java.util.logging.Level.INFO
            case Level.Debug => java.util.logging.Level.FINE
          }, 
          message
        )
      }
    }
  }

  trait ConsoleIO[F[_]] {
    def println(line: String): F[Unit]
    def readln: F[String]
  }

  object ConsoleIO {
    def apply[F[_]](implicit F: ConsoleIO[F]): ConsoleIO[F] = F
  }

  sealed trait Level
  object Level {
    case object Info extends Level
    case object Debug extends Level
  }

  trait Logging[F[_]] {
    def log(level: Level)(message: => String): F[Unit]
  }

  object Logging {
    def apply[F[_]](implicit F: Logging[F]): Logging[F] = F
  }

  implicit val pool: ForkJoinPool = new ForkJoinPool(2)

  // object Console {
  //   def println(line: String): IO[Exception, Unit] = IO.sync(scala.Console.println(line))

  //   val readln: IO[Exception, String] = IO.sync(scala.io.StdIn.readLine())
  // }

  def main[F[_] : Monad : ConsoleIO : Logging]: F[String] = {
    for {
      joinP1 <- ConsoleIO[F].println("Hello player! What's your name?")
      joinP2 <- ConsoleIO[F].println("I'm writing concurrently here!")
      n <- ConsoleIO[F].readln
      _ <- Logging[F].log(Level.Info)(n)
      _ <- ConsoleIO[F].println(s"Hello, $n, welcome to the game!")
    } yield n
  }

  val mainIO: IO[Exception, String] = main[IO[Exception, ?]]

  type IOE[A] = IO[Exception, A]

  val concIO: IOE[Unit] = for {
    joinP1 <- ConsoleIO[IOE].println("Hello player! What's your name?").fork.mapError(_.absurd[Exception])
    joinP2 <- ConsoleIO[IOE].println("I'm writing concurrently here!").fork.mapError(_.absurd[Exception])
    _ <- joinP1
    _ <- joinP2
  } yield () 

  /*
  *
  * Lenses
  *
  */

  case class Lens[S, A](
    set: A => S => S,
    get : S => A
  ) { self =>
    def >>> [B](that: Lens[A, B]): Lens[S, B] = {
      val thatSet: B => S => S = (b: B) => (s: S) => {
        self.set(that.set(b)(self.get(s)))(s)
      }

      val thatGet: S => B = s => that.get(get(s))
      Lens(
        set = thatSet,
        get = thatGet
      )
    }
  }

  case class Prism[S, A](set: A => S, get: S => Option[A]) { self =>
    def >>> [B](that: Prism[A, B]): Prism[S, B] =
      Prism[S, B](
        set = b => set(that.set(b)),
        get = s => get(s).flatMap(that.get)
      )
  }

  /*
  *
  * GAME
  *
  */

  case class Character(
    position: (Int, Int)
  )

  sealed trait Item

  sealed trait Cell {
    def look: String
    def toChar: Char
  }
  case class Land() extends Cell {
    def look = "Land"
    def toChar = '_'
  }
  case class Sea() extends Cell {
    def look = "Sea"
    def toChar = '~'
  }

  case class GameMap(world: Vector[Vector[Cell]]) {
    def cellAt(tuple: (Int, Int)): Option[Cell] = {
      val (x, y) = tuple

      for {
        row <- world.lift(x)
        cell <- row.lift(y)
      } yield cell
    }
  }

  case class GameWorld(
    player: Character,
    map: GameMap
  ) {
    def render: String = {
      val mapRender: Vector[Vector[Char]] = map.world.map(_.map(_.toChar))

      val row = mapRender(player.position._1)
      val updatedRow = row.updated(player.position._2, 'X')

      val result = mapRender.updated(player.position._1, updatedRow)

      "\n" + result.map(_.mkString("|")).mkString("\n") + "\n"
    }

  }

  sealed trait Command
  case class Attack(enemy: Character) extends Command
  case class Move(direction: Direction) extends Command
  case class Look(what: Option[Either[Item, Character]]) extends Command
  case object Exit extends Command

  object Command {
    val _Move = Prism[Command, Direction](Move.apply, {
      case Move(d) => Some(d)
      case _ => None
    })
  }

  sealed trait Direction
  case object North extends Direction
  case object South extends Direction
  case object West extends Direction
  case object East extends Direction

  object Direction {
    val _North = Prism[Direction, Unit](_ => North, {
      case North => Some(())
      case _ => None
    })
    val _South = Prism[Direction, Unit](_ => South, {
      case South => Some(())
      case _ => None
    })
    val _West = Prism[Direction, Unit](_ => West, {
      case West => Some(())
      case _ => None
    })
    val _East = Prism[Direction, Unit](_ => East, {
      case East => Some(())
      case _ => None
    })
  }

  def parse(line: String): Either[String, Command] = {
    val words = line.split("\\s+")

    if (words.length == 0) Right(Look(None))
    else {
      words(0) match {
        case "look" => Right(Look(None))
        case "go" | "move" =>
          if (words.length < 2) Left("Specify direction to move")
          else words(1) match {
            case "north" => Right(Move(North))
            case "south" => Right(Move(South))
            case "west" => Right(Move(West))
            case "east" => Right(Move(East))
            case _ => Left("Incorrect direction")
          }
        case "q" => Right(Exit)
        case _ => Left(line)
      }
    }
  }

  case class Update(message: String, next: Option[GameWorld])

  def update(command: Command, world: GameWorld): Update = command match {
    case Look(None) => {
      val cellOpt = world.map.cellAt(world.player.position)
      cellOpt.map(c => Update(world.render, Option(world)))
        .getOrElse(Update("You're in nowhere!", None))
    }
    case Look(Some(x)) => Update(world.render, Option(world))
    case Move(direction) => 
      val (newY, newX) = direction match {
        case North => (world.player.position._1 - 1, world.player.position._2)
        case South => (world.player.position._1 + 1, world.player.position._2)
        case West => (world.player.position._1, world.player.position._2 - 1)
        case East => (world.player.position._1, world.player.position._2 + 1)
      }
      val cellOpt = world.map.cellAt((newY, newX))
      cellOpt.map(cell => {
          val newWorld = world.copy(player = world.player.copy(position = (newY, newX)))
          Update(newWorld.render, Option(newWorld))
        }
      ).getOrElse(Update("You can't go there", Option(world)))
    case Attack(enemy) => Update("...", Option(world))
    case Exit => Update("Bye, bye!", None)
  }

  def gameStep[F[_]: Monad: ConsoleIO](world: GameWorld): F[Option[GameWorld]] = {
    for {
      input <- ConsoleIO[F].readln
      nextWorldOpt <- parse(input) match {
        case Left(err) => ConsoleIO[F].println(err).map(_ => Option(world))
        case Right(command) => 
          val Update(message, nextWorldOpt) = update(command, world)
          ConsoleIO[F].println(message).map(_ => nextWorldOpt)
       }
     } yield nextWorldOpt
  }

  def gameLoop[F[_]: Monad: ConsoleIO](world: GameWorld): F[Unit] =
    gameStep(world).flatMap {
      case None => Monad[F].point(())
      case Some(newWorld) => gameLoop(newWorld)
    }

  def gameStart[F[_]: Monad: ConsoleIO]: F[Unit] = {
    val map = Vector(
      Vector(Land(), Land(), Sea()),
      Vector(Land(), Sea(), Sea()),
      Vector(Land(), Land(), Land())
    )
    val startWorld = GameWorld(Character((0, 0)), GameMap(map))

    for {
      _ <- ConsoleIO[F].println("... Welcome to the GAME ...")
      _ <- gameLoop(startWorld)
    } yield ()
  }

  val gameStartIO = gameStart[IO[Exception, ?]]

}
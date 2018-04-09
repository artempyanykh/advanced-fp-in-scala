import scala.concurrent.Future

object example {
  type Parser[A] = String => Either[String, (A, String)]

  val char: Parser[Char] = input =>
   input.headOption.map(c => Right((c, input.drop(1)))).getOrElse(Left("No more input!"))

  def fail[A](error: String): Parser[A] = input => Left(error)

  def alt[A](l: Parser[A], r: Parser[A]): Parser[A] = input => l(input) match {
    case Left(_) => r(input)
    case x => x
  }

  def seq[A, B](fst: Parser[A], f: A => Parser[B]): Parser[B] = input => fst(input) match {
    case Left(err) => Left(err)
    case Right((a, rem)) => f(a)(rem)
  }

  def pure[A](a: A): Parser[A] = input => Right((a, input))

  def map[A, B](fa: Parser[A], f: A => B): Parser[B] = input => fa(input) match {
    case Left(err) => Left(err)
    case Right((a, rem)) => Right((f(a), rem))
  }

  def pipe[A, B, C](fst: A => B)(snd: B => C): A => C = fst andThen snd
  
  def identity[A](a: A): A = a

  // object identity {
  //   def apply[A](a: A): A = a
  // }

  case class Player(
    name: String,
    inventory: List[Item],
    level: Int,
    position: (Int, Int)
  )

  sealed trait Item

  sealed trait Cell
  case class Land() extends Cell
  case class Sea() extends Cell

  case class GameMap(world: Vector[Vector[Cell]])

  case class GameWorld(
    player: Player,
    map: GameMap
  )

  def filter[F[_], A](xs: F[A]): F[A] = ???

  trait StackLike[Stack[_]] {
    def empty[A]: Stack[A]
    def push[A](a: A, s: Stack[A]): Stack[A]
  }

  trait Algorithm[Stack[_]] {
    def apply(implicit S: StackLike[Stack]): Stack[Int] = ???
  }

  trait Module[Alg[_[_]]] {

  }

  type MyModule = Module[Algorithm]

  val plus: (Int, Int) => Int = (x, y) => x + y

  val increment: Int => Int = plus(1, _)

  trait Foldable[F[_]] {
    def fold[Z, A](fa: F[A])(z: Z)(f: (Z, A) => Z): Z
  }

  val ListFoldable = new Foldable[List] {
    def fold[Z, A](fa: List[A])(z: Z)(f: (Z, A) => Z): Z = fa.foldLeft(z)(f)
  }

  def MapFoldable[K] = new Foldable[Map[K, ?]] {
    def fold[Z, A](fa: Map[K, A])(z: Z)(f: (Z, A) => Z): Z = fa.values.foldLeft(z)(f)
  }

  trait Appendable[A] {
    def append(a: A, b: A): A
  }

  def foo[A](x: A)(implicit A: Appendable[A]): A = x
  // A.append(x, x)
  // A.append(A.append(x, x), x)

  // def zip[F[_]: Apply, A, B, C](a: F[A], b: F[B])
  //   (f: A \&/ B)
  //   (zero: C, smash: (C, C) => C): List[C]

  sealed trait Ordering
  case object LT extends Ordering
  case object GT extends Ordering
  case object EQ extends Ordering

  // typeclass
  trait Ord[A] {
    def compare(l: A, r: A): Ordering
  }

  object Ord {
    def apply[A](implicit ord: Ord[A]): Ord[A] = ord

    implicit class OrdSyntax[A](l: A) {
      def =?= (r: A)(implicit O: Ord[A]): Ordering = O.compare(l, r)
    }

    implicit val IntOrd = new Ord[Int] {
      def compare(l: Int, r: Int): Ordering = if (l < r) LT else if (l > r) GT else EQ
    }

    implicit def ListOrd[A: Ord] = new Ord[List[A]] {
      def compare(l: List[A], r: List[A]): Ordering = (l, r) match {
        case (Nil, Nil) => EQ
        case (Nil, _) => LT
        case (_, Nil) => GT
        case (x :: xs, y :: ys) => Ord[A].compare(x, y) match {
          case LT => LT
          case GT => GT
          case EQ => compare(xs, ys)
        }
      }
    }
  }

  def sort[A: Ord](list: List[A]): List[A] = list match {
    case Nil => Nil
    case x :: xs =>
      val (lessThan, notLessThan) = xs.partition(y => Ord[A].compare(y, x) == LT)

      sort(lessThan) ++ (x :: Nil) ++ sort(notLessThan)
  }

  case class Room(name: String, number: Int)

  object Room {
    implicit val ord: Ord[Room] = new Ord[Room] {
      def compare(l: Room, r: Room): Ordering =
        if (l.name < r.name) LT
        else if (l.name > r.name) GT
        else {
          if (l.number < r.number) LT
          else if (l.number > r.number) GT
          else EQ
        }
    }
  }

  // when there are more than 1 instance, use newtype
  class ReverseRoom(val room: Room) extends AnyVal
  object ReverseRoom {
    // put your alternative implicit instances here
  }

  trait Semigroup[A] {
    // associativity
    def append(a1: A, a2: A): A
  }

  trait Monoid[A] extends Semigroup[A] {
    // append(_, zero) == append(0, zero) == a
    def zero: A
  }

  // // come up with a typeclass and rules
  // trait RepeatableOp[A] {
  //   // op(op(a)) == op(a)
  //   def op(a: A): A
  // }

  // implicit def MapRepeatablePut[Int, Int] = Repeatable[Map[K, V]] {
  //   def op(a: Map[K, V]): Map[K, V] = a ++ (0, 0)
  // }

  trait Functor[F[_]] {
    def map[A, B](ab: A => B): F[A] => F[B]
  }

  def Function1Functor[A] = new Functor[A => ?] {
    def map[B, C](bc: B => C): (A => B) => (A => C) = fa => fa andThen bc
  }

  case class Identity[A](run: A)

  val IdentityFunctor: Functor[Identity] = new Functor[Identity] {
    def map[A, B](ab: A => B): Identity[A] => Identity[B] = i => Identity(ab(i.run))
  }

  trait NaturalTransformation[F[_], G[_]] {
    def apply[A](fa: F[A]): G[A]
  }

  type ~>[F[_], G[_]] = NaturalTransformation[F, G]

  val ListToOption: List ~> Option = new NaturalTransformation[List, Option] {
    def apply[A](la: List[A]): Option[A] = la.headOption
  }

  case class Composite[F[_], G[_], A](run: F[G[A]])

  def CompositeFunctor[F[_]: Functor, G[_]: Functor] = new Functor[Composite[F, G, ?]] {
    def map[A, B](ab: A => B): Composite[F, G, A] => Composite[F, G, B] = fga => {
      val mapping: F[G[A]] => F[G[B]] =
        implicitly[Functor[F]].map(implicitly[Functor[G]].map(ab))
      Composite(mapping(fga.run))
    }
  }

  trait Monad[M[_]] {
    def pure[A](a: A): M[A]
    def flatMap[A, B](ma: M[A])(abo: A => M[B]): M[B]
  }


  final case class OptionT[F[_], A](run: F[Option[A]]) {
    def map[B](f: A => B)(implicit F: Functor[F]): OptionT[F, B] = 
      OptionT(
        F.map((v: Option[A]) => v.map(f))(run)
      )

    def flatMap[B](f: A => OptionT[F, B])(implicit M: Monad[F]): OptionT[F, B] = {
      OptionT(
        M.flatMap(run)({
         case None => M.pure(Option.empty[B])
         case Some(a) => f(a).run
        })
      )
    }
  }

  type FutureOption[A] = OptionT[Future, A]

}
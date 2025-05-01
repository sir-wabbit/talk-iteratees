import java.io.File

import cats.{Monad, Eval}
import io.iteratee._
import io.iteratee.files.either._

sealed trait Iteratee[F[_], E, A] {
  def run(implicit F: Monad[F]): F[A] = this match {
    case Done(a) => a
    case Cont(next) => F.flatMap(next(None))(_.run)
  }
}
final case class Done[F[_], E, A](a: F[A]) extends Iteratee[F, E, A]
final case class Cont[F[_], E, A](next: Option[E] => F[Iteratee[F, E, A]]) extends Iteratee[F, E, A]

object Iteratee {
  implicit def instance[F[_], E](F: Monad[F]): Monad[Iteratee[F, E, ?]] = new Monad[Iteratee[F, E, ?]] {
    override def pure[A](x: A): Iteratee[F, E, A] = Done[F, E, A](F.pure(x))
    override def flatMap[A, B](ifa: Iteratee[F, E, A])(f: (A) => Iteratee[F, E, B]): Iteratee[F, E, B] =
      F.map(ifa.run(F))(f)
    override def tailRecM[A, B](a: A)(f: (A) => Iteratee[F, E, Either[A, B]]): Iteratee[F, E, B] = ???
  }
}

sealed trait Enumerator[F[_], E] {
  def apply[A](i: Iteratee[F, E, A])(implicit F: Monad[F]): F[Iteratee[F, E, A]]
}
object Enumerator {
  def enumList[F[_], E](list: List[E]): Enumerator[F, E] = new Enumerator[F, E] {
    override def apply[A](i: Iteratee[F, E, A])(implicit F: Monad[F]): F[Iteratee[F, E, A]] = list match {
      case Nil => F.pure(i)
      case x :: xs => i match {
        case Cont(next) => F.flatMap(next(Some(x)))(n => enumList(xs).apply(n))
        case Done(a) => F.pure(Done[F, E, A](a))
      }
    }
  }
}

object App {
  def pureMain(args: List[String]): Eval[Unit] = args match {
    case f1 :: f2 :: Nil =>
      ???
    case _ => Eval.later(println("Please supply two command line arguments."))
  }

  def main(args: Array[String]): Unit = pureMain(args.toList).value
}

import cats.functor.{Profunctor, Contravariant}
import cats.{Functor, Applicative, Monad}
import cats.syntax.all._
import cats.instances.either._

import scala.annotation.tailrec

package object basic2 {
  trait Forall[F[_]] {
    def apply[A]: F[A]
  }

  final case class Iteratee[F[_], E, A](unwrap: F[Either[Option[E] => Iteratee[F, E, A], A]])

  object Iteratee {
    def wrap[F[_], E, A](unwrap: F[Either[Option[E] => Iteratee[F, E, A], A]]): Iteratee[F, E, A] =
      Iteratee[F, E, A](unwrap)

    def done[F[_], E, A](a: A)(implicit F: Applicative[F]): Iteratee[F, E, A] =
      Iteratee[F, E, A](F.pure(Either.right(a)))
    def cont[F[_], E, A](k: Option[E] => Iteratee[F, E, A])(implicit F: Applicative[F]): Iteratee[F, E, A] =
      Iteratee[F, E, A](F.pure(Either.left(k)))
    def fail[F[_], E, A](implicit F: Applicative[F]): Iteratee[F, E, A] =
      cont[F, E, A](_ => fail(F))

    implicit def monad[F[_], E](implicit F: Monad[F]): Monad[Iteratee[F, E, ?]] = new Monad[Iteratee[F, E, ?]] {
      override def pure[A](x: A): Iteratee[F, E, A] = done(x)

      override def flatMap[A, B](it: Iteratee[F, E, A])(f: A => Iteratee[F, E, B]): Iteratee[F, E, B] =
        wrap[F, E, B](it.unwrap.flatMap {
          case Right(a) => f(a).unwrap
          case Left(k) => F.pure(Either.left(a => flatMap(k(a))(f)))
        })

      override def tailRecM[A, B](a: A)(f: A => Iteratee[F, E, Either[A, B]]): Iteratee[F, E, B] =
        flatMap(pure(Either.left(a))) {
          case Left(x) => tailRecM(x)(f)
          case Right(x) => pure(x)
        }
    }

    implicit def contravariant[F[_], X](implicit F: Functor[F]): Contravariant[Iteratee[F, ?, X]] = new Contravariant[Iteratee[F, ?, X]] {
      override def contramap[A, B](fa: Iteratee[F, A, X])(f: (B) => A): Iteratee[F, B, X] =
        Iteratee[F, B, X](F.map(fa.unwrap) {
          case Left(k) => Either.left(b => contramap(k(b.map(f)))(f))
          case Right(x) => Either.right(x)
        })
    }

    def profunctor[F[_]](implicit F: Functor[F]): Profunctor[Iteratee[F, ?, ?]] = new Profunctor[basic2.Iteratee[F, ?, ?]] {
      override def dimap[A, B, C, D](fab: Iteratee[F, A, B])(f: (C) => A)(g: (B) => D): Iteratee[F, C, D] =
        Iteratee[F, C, D](F.map(fab.unwrap) {
          case Left(k) => Either.left(c => dimap[A, B, C, D](k(c.map(f)))(f)(g))
          case Right(x) => Either.right(g(x))
        })
    }
  }

  final case class Enumerator[F[_], E](run: Forall[λ[A => Iteratee[F, E, A] => Iteratee[F, E, A]]])
  object Enumerator {

  }

  final case class Enumeratee[F[_], O, I](run: Forall[λ[A => Iteratee[F, I, A] => Iteratee[F, O, Iteratee[F, I, A]]]])
  object Enumeratee {

  }

  object syntax {
    implicit final class IterateeOps[F[_], E, A](val it: Iteratee[F, E, A]) extends AnyVal {
      def run(implicit F: Monad[F]): F[A] = F.flatMap(it.unwrap) {
        case Right(a) => F.pure(a)
        case Left(k) => k(None).run(F)
      }


    }
  }

  def enumList[F[_], E](list: List[E]): Enumerator[F, E] = {
    @tailrec def go[A](list: List[E], it: Iteratee[F, E, A]): Iteratee[F, E, A] = ???

    Enumerator(new Forall[λ[A => Iteratee[F, E, A] => Iteratee[F, E, A]]] {
      override def apply[A]: Iteratee[F, E, A] => Iteratee[F, E, A] = it => go(list, it)
    })
  }
}

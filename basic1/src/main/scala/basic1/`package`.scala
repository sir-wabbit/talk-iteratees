package basic1

import cats.functor.Contravariant
import cats.{Functor, Monad}
import cats.kernel.Monoid
import cats.syntax.all._
import cats.instances.int._

import scala.annotation.tailrec

object `package` {
  sealed abstract class Iteratee[E, A] {
    def fold[Z](ifDone: A => Z, ifCont: (Option[E] => Iteratee[E, A]) => Z): Z = this match {
      case Done(a) => ifDone(a)
      case Cont(k) => ifCont(k)
    }

    def isDone: Boolean = fold(_ => true, _ => false)
  }
  final case class Done[E, A](a: A) extends Iteratee[E, A]
  final case class Cont[E, A](k: Option[E] => Iteratee[E, A]) extends Iteratee[E, A]

  // trait Forall[F[_]] {
  //   def apply[A]: F[A]
  // }
  // type Enumerator[E] = Forall[λ[A => Iteratee[E, A] => Iteratee[E, A]]]
  // type Enumeratee[O, I] = Forall[λ[A => Iteratee[I, A] => Iteratee[O, Iteratee[I, A]]]]

  sealed abstract class Enumerator[E] {
    def apply[A](it: Iteratee[E, A]): Iteratee[E, A]
  }
  sealed abstract class Enumeratee[O, I] {
    def apply[A](it: Iteratee[I, A]): Iteratee[O, Iteratee[I, A]]

    def into[A](it: Iteratee[I, A]): Iteratee[O, A] = monad.map(apply(it))(run)
  }

  @tailrec def run[E, A](it: Iteratee[E, A]): A = it match {
    case Done(a) => a
    case Cont(k) => run(k(None))
  }

  def enumList[E](list: List[E]): Enumerator[E] = {
    @tailrec def go[A](list: List[E], it: Iteratee[E, A]): Iteratee[E, A] = list match {
      case Nil => it
      case x :: xs => it match {
        case Done(a) => Done(a)
        case Cont(k) => go(xs, k(Some(x)))
      }
    }

    new Enumerator[E] {
      override def apply[A](it: Iteratee[E, A]): Iteratee[E, A] = go(list, it)
    }
  }

  def empty[E, A](a: A): Iteratee[E, A] = Done(a)
  def failure[E, A]: Iteratee[E, A] = Cont(_ => failure)

  implicit def contravariant[X]: Contravariant[Iteratee[?, X]] = new Contravariant[Iteratee[?, X]] {
    override def contramap[A, B](fa: Iteratee[A, X])(f: B => A): Iteratee[B, X] = fa match {
      case Done(x) => Done(x)
      case Cont(k) => Cont(b => contramap(k(b.map(f)))(f))
    }
  }

  implicit def monad[E]: Monad[Iteratee[E, ?]] = new Monad[Iteratee[E, ?]] {
    override def pure[A](x: A): Iteratee[E, A] = empty(x)

    override def map[A, B](fa: Iteratee[E, A])(f: (A) => B): Iteratee[E, B] = fa match {
      case Done(a) => Done(f(a))
      case Cont(k) => Cont(e => map(k(e))(f))
    }

    override def flatMap[A, B](fa: Iteratee[E, A])(f: (A) => Iteratee[E, B]): Iteratee[E, B] = fa match {
      case Done(x) => f(x)
      case Cont(k) => Cont(v => flatMap(k(v))(f))
    }

    // Unsafe!
    override def tailRecM[A, B](a: A)(f: (A) => Iteratee[E, Either[A, B]]): Iteratee[E, B] =
        flatMap(f(a))(_.fold(x => tailRecM(x)(f), pure))
  }

  def maybeOne[E]: Iteratee[E, Option[E]] =
    Cont(x => Done(x))
  def one[E]: Iteratee[E, E] =
    monad.flatMap(maybeOne[E])(_.fold(failure[E, E])(x => empty[E, E](x)))
  def sat[E](p: E => Boolean): Iteratee[E, E] =
    monad.flatMap(one[E])(e => if (p(e)) empty(e) else failure)

  def sum[E](implicit M: Monoid[E]): Iteratee[E, E] = {
    def go(acc: E): Iteratee[E, E] = Cont {
      case None => Done(acc)
      case Some(x) => go(M.combine(acc, x))
    }
    go(M.empty)
  }

  def filter[E](p: E => Boolean): Enumeratee[E, E] = new Enumeratee[E, E] {
    override def apply[A](it: Iteratee[E, A]): Iteratee[E, Iteratee[E, A]] = {
      def go(e: Option[E], it: Iteratee[E, A]): Iteratee[E, Iteratee[E, A]] = it match {
        case Done(x) => Done(it)
        case Cont(k) => Cont {
          case None => Done(k(None))
          case Some(x) if p(x) => Done(k(Some(x)))
          case Some(x) => Cont(x => go(x, it))
        }
      }

      Cont(x => go(x, it))
    }
  }

  implicit class IterateeOps[E, A](val value: Iteratee[E, A]) extends AnyVal {
    def <|(other: => Iteratee[E, A]): Iteratee[E, A] = value match {
      case Done(a) => Done(a)
      case Cont(k1) =>
        other match {
          case Done(a) => Done(a)
          case Cont(k2) => Cont(x => k1(x) <| k2(x))
        }
    }
  }
}

object App {
  def main(args: Array[String]): Unit = {
    println(enumList(List(1, 2, 3, 4)).apply(filter[Int](_ % 2 == 0).into(sum[Int])))
  }
}

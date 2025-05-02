import java.io.{File, FileInputStream}

import cats.data.EitherT
import io.iteratee.Enumerator

import scala.util.control.NonFatal

class App {
  // Regular Java IO.
  //
  // Pros:
  //  * Memory consumption.
  //
  // Cons:
  //  * Composability.
  //  * Exception and resource handling.
  //  * Side effects.
  def javaCountWS(is: FileInputStream): Int = {
    var count = 0
    while (true) {
      try {
        val ch = is.read()
        if (ch == -1) return 0
        else if (Character.isSpaceChar(ch)) count += 1
      } catch {
        case NonFatal(e) => throw e
      }
    }
    count
  }

  def javaRun(f: File): Unit = {
    try {
      val is = new FileInputStream(f)
      try {
        println(javaCountWS(is))
      } catch {
        case NonFatal(e) => e.printStackTrace()
      }
      is.close()
    } catch {
      case NonFatal(e) => e.printStackTrace()
    }
  }

  // Scala Iterator IO.
  //
  // Pros:
  //  * Memory consumption.
  //  * Composability.
  //
  // Cons:
  //  * Exception and resource handling.
  //  * Side effects.
  def iteratorCountWS(is: Iterator[Char]): Int =
    is.filter(_.isSpaceChar).map(_ => 1).sum

  def iteratorRun(f: File): Unit = try {
    val is = scala.io.Source.fromFile(f)
    try {
      println(iteratorCountWS(is.iter))
    } catch {
      case NonFatal(e) => e.printStackTrace()
    }
    is.close()
  } catch {
    case NonFatal(e) => e.printStackTrace()
  }

  // Scala HORRIFYINGLY BAD IO (aka Lazy IO).
  //
  // Pros:
  //  * Composability.
  //
  // Cons:
  //  * Memory consumption.
  //  * Exception and resource handling.
  //  * Side effects.
  def lazyCountWS(is: Stream[Char]): Int =
    is.filter(_.isSpaceChar).map(_ => 1).sum

  def lazyRun(f: File): Unit = try {
    val is = scala.io.Source.fromFile(f)
    try {
      println(lazyCountWS(is.iter.toStream))
    } catch {
      case NonFatal(e) => e.printStackTrace()
    }
    is.close()
  } catch {
    case NonFatal(e) => e.printStackTrace()
  }

  // Iteratee IO.
  // Cons:
  //  * Type annotations.
  //
  // Pros:
  //  * Composability.
  //  * Memory consumption.
  //  * Exception and resource handling.
  //  * Side effects.

  import cats.Eval
  import cats.syntax.all._
  import cats.instances.int._
  import io.iteratee.Iteratee
  import io.iteratee.modules.eitherT._
  import io.iteratee.files.eitherT.readBytes

  type IO[A] = EitherT[Eval, Throwable, A]
  def readChars(file: File): Enumerator[IO, Char] =
    readBytes(file).flatMap(b => enumIndexedSeq(b).map(_.toChar))

  def iterateeCountWS: Iteratee[IO, Char, Int] =
    sum.through(filter[Char](_.isSpaceChar).map(_ => 1))

  def iterateeRun(file: File): Eval[Unit] =
    readChars(file).into(iterateeCountWS).fold(
      e => Eval.later(e.printStackTrace()),
      x => Eval.later(println(x)))
}

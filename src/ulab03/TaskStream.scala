package ulab03

import u03.Streams.Stream
import u03.Streams.Stream.{Cons, empty}

import scala.annotation.tailrec


object TaskStream {

  @tailrec
  def drop[A](stream: Stream[A])(n: Int): Stream[A] = (stream, n) match {
    case _ if n<1 || stream == empty() => stream
    case (Cons(_, t), n) => drop(t())(n-1)
  }
}
package ulab03
import u03.Lists.List.{Cons, append}
import u03.Lists._

import scala.annotation.tailrec

object Task1 {
  @tailrec
  def drop[A](l: List[A], n: Int): List[A] = l match {
    case _ if n<1 || l == List.Nil() => l
    case Cons(_, t)  => drop(t, n-1)
  }

  def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] = l match {
    case Cons(h, t) => append(f(h), flatMap(t)(f))
    case _ => List.Nil()
  }
}

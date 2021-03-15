package ulab03
import u03.Lists.List.Cons
import u03.Lists._

import scala.annotation.tailrec

object Task1 {
  @tailrec
  def drop[A](l: List[A], n: Int): List[A] = l match {
    case _ if n<1 || l == List.Nil() => l
    case Cons(_, t)  => drop(t, n-1)
  }
}

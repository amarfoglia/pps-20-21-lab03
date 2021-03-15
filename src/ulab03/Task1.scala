package ulab03
import u03.Lists.List.{Cons, Nil, append}
import u03.Lists._

import scala.annotation.tailrec

object Task1 {
  @tailrec
  def drop[A](l: List[A], n: Int): List[A] = l match {
    case _ if n<1 || l == Nil() => l
    case Cons(_, t)  => drop(t, n-1)
  }

  def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] = l match {
    case Cons(h, t) => append(f(h), flatMap(t)(f))
    case _ => Nil()
  }

  def map[A,B](l: List[A])(mapper: A=>B): List[B] = flatMap(l)(x => Cons(mapper(x), Nil()))

  def filter[A](l1: List[A])(pred: A=>Boolean): List[A] = l1 match {
    case _  => flatMap(l1)(x => if(pred(x)) Cons(x, Nil()) else Nil())
//    case Cons(h,t) if (pred(h)) => Cons(h, filter(t)(pred))
//    case Cons(_,t) => filter(t)(pred)
//    case Nil() => Nil()
  }
}

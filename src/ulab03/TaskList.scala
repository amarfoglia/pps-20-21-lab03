package ulab03
import u02.SumTypes.{Person, Teacher}
import u03.Lists.List.{Cons, Nil, append}
import u03.Lists._

import scala.annotation.tailrec

object TaskList {
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

  def filter[A](l: List[A])(pred: A=>Boolean): List[A] = flatMap(l)(x => if(pred(x)) Cons(x, Nil()) else Nil())

  def max(l: List[Int]): Option[Int] = l match {
    case Cons(h, t) => Some(Math.max(h, max(t).getOrElse(Int.MinValue)))
    case Nil() => None
  }

  def peopleToCourses(people: List[Person]): List[String] = flatMap(people) {
    case Teacher(_, course) => Cons(course, Nil())
    case _ => Nil()
  }

  @tailrec
  def foldLeft[A,B](l: List[A])(acc: B)(op: (B, A) => B): B = l match {
    case Cons(h, t) => foldLeft(t)(op(acc, h))(op)
    case Nil() => acc
  }

  def foldRight[A,B](l: List[A])(acc: B)(op: (A, B) => B): B = l match {
    case Cons(h, t) => op(h, foldRight(t)(acc)(op))
    case Nil() => acc
  }

  def foldRightViaFoldLeft[A,B](l: List[A])(acc: B)(op: (A, B) => B): B =
    foldLeft(reverse(l))(acc)((a, b) => op(b, a))

  private def reverse[A](l: List[A]): List[A] = l match {
    case Cons(h, t) => append(reverse(t), Cons(h, Nil()))
    case _ => l
  }
}

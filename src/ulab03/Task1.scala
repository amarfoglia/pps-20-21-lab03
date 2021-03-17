package ulab03
import u02.SumTypes.{Person, Teacher}
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
  def foldLeft[A,B](l: List[A])(acc: B)(operator: (B, A) => B): B = l match {
    case Cons(h, t) => foldLeft(t)(operator(acc, h))(operator)
    case Nil() => acc
  }

  def foldRight[A,B](l: List[A])(acc: B)(operator: (A, B) => B): B = l match {
    case Cons(h, t) => operator(h, foldRight(t)(acc)(operator))
    case Nil() => acc
  }
}

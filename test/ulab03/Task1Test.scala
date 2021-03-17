package ulab03
import org.junit.jupiter.api.Assertions.{assertEquals, assertTrue}
import u03.Lists._
import org.junit.jupiter.api.Test
import u02.SumTypes.{Person, Student, Teacher}
import u03.Lists.List.{Cons, append}
import ulab03.Task1.{drop, filter, flatMap, foldLeft, foldRight, map, max, peopleToCourses}

class Task1Test {
  val lst: List[Int] = Cons(10, Cons(20, Cons(30, List.Nil())))

  @Test
  def testDrop(): Unit = {
    assertEquals(Cons(20,Cons(30, List.Nil())), drop(lst ,1))
    assertEquals(Cons(30, List.Nil()), drop(lst ,2))
    assertEquals(List.Nil(), drop(lst , 5))
  }

  @Test
  def testFlatMap(): Unit = {
    assertEquals(Cons(11,Cons(21,Cons(31, List.Nil()))), flatMap(lst)(v => Cons(v+1, List.Nil())))
    assertEquals(Cons(11,Cons(12,Cons(21,Cons(22,Cons(31,Cons(32, List.Nil())))))),
      flatMap(lst)(v => Cons(v+1, Cons(v+2, List.Nil()))))
  }

  @Test
  def testMapBasedOnFlatMap(): Unit = {
    assertEquals(Cons(11,Cons(21,Cons(31, List.Nil()))), map(lst)(_+1))
    assertEquals(Cons(20,Cons(40,Cons(60, List.Nil()))), map(lst)(_*2))
    assertEquals(Cons(-10.0, Cons(-20.0, Cons(-30.0, List.Nil()))), map(lst)(_*(-1.0)))
  }

  @Test
  def testFilterBasedOnFlatMap(): Unit = {
    assertEquals(lst, filter(lst)(_>0))
    assertEquals(Cons(10, Cons(20, List.Nil())), filter(lst)(_<30))
    assertEquals(List.Nil(), filter(lst)(_<0))
  }

  @Test
  def testMax(): Unit = {
    assertEquals(Some(30), max(lst))
    assertEquals(Some(-10), max(map(lst)(_*(-1))))
    assertEquals(Some(15), max(Cons(5, Cons(15, Cons(10, List.Nil()))))) // test max value in the middle
    assertTrue(max(List.Nil()).isEmpty)
  }

  @Test
  def testPeopleToCourses(): Unit = {
    val students: List[Person] = Cons(Student("jacob", 1998), Cons(Student("James", 1997), List.Nil()))
    val teachers: List[Person] = Cons(Teacher("john", "pps"), Cons(Teacher("jake", "pcd"), List.Nil()))
    val courses = Cons("pps", Cons("pcd", List.Nil()))
    assertEquals(courses, peopleToCourses(append(students, teachers)))
    assertEquals(courses, peopleToCourses(append(teachers, students)))
    assertEquals(List.Nil(), peopleToCourses(students))
  }

  @Test
  def testFoldLeft(): Unit = {
    assertEquals(60, foldLeft(lst)(0)(_+_))
    assertEquals(-60, foldLeft(lst)(0)(_-_))
  }

  @Test
  def testFoldRight(): Unit = {
    val lst2 = Cons(3, Cons(7, Cons(1, Cons(5, List.Nil ()))))
    assertEquals(-8, foldRight(lst2) (0) (_ - _))
    assertEquals(60, foldRight(lst)(0)(_+_))
    assertEquals(20, foldRight(lst)(0)(_-_))
  }
}
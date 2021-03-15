package ulab03
import org.junit.jupiter.api.Assertions.assertEquals
import u03.Lists._
import org.junit.jupiter.api.Test
import u03.Lists.List.Cons
import ulab03.Task1.{drop, flatMap}

class Task1Test {
  val lst: Cons[Int] = Cons(10, Cons(20, Cons(30, List.Nil())))

  @Test
  def testDrop(): Unit = {
    assertEquals(Cons(20,Cons(30, List.Nil())), drop(lst ,1))
    drop(lst ,2)
    drop(lst ,5)
  }

  @Test
  def testFlatMap(): Unit = {
    assertEquals(Cons(11,Cons(21,Cons(31, List.Nil()))), flatMap(lst)(v => Cons(v+1, List.Nil())))
    assertEquals(Cons(11,Cons(12,Cons(21,Cons(22,Cons(31,Cons(32, List.Nil())))))),
      flatMap(lst)(v => Cons(v+1, Cons(v+2, List.Nil()))))
  }
}

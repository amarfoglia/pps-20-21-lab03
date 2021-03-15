package ulab03
import org.junit.jupiter.api.Assertions.assertEquals
import u03.Lists._
import org.junit.jupiter.api.Test
import u03.Lists.List.Cons
import ulab03.Task1.drop

class Task1Test {
  @Test
  def testDrop(): Unit = {
    val  lst = Cons(10, Cons(20, Cons(30, List.Nil())))
    assertEquals(Cons(20,Cons(30, List.Nil())), drop(lst ,1))
    drop(lst ,2)
    drop(lst ,5)
  }
}

package ulab03

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test
import u03.Lists.List.{Cons, Nil, append}
import u03.Lists.List
import u03.Streams.Stream
import u03.Streams.Stream.{empty, toList}
import ulab03.TaskStream.{constant, drop}

class TaskStreamTest {
  val STREAM_SIZE = 10
  val s: Stream[Int] = Stream.take(Stream.iterate(0)(_+1))(STREAM_SIZE)

  @Test
  def testDrop(): Unit = {
    assertEqualsStreams(Stream.take(Stream.iterate(6)(_+1))(4), drop(s)(6))
    assertEqualsStreams(empty(), drop(s)(STREAM_SIZE))
    assertEqualsStreams(empty(), drop(s)(STREAM_SIZE+1))
    assertEqualsStreams(s, drop(s)(0))
  }

  private def assertEqualsStreams[A](expected: Stream[A], actual: Stream[A]): Unit =
    assertEquals(toList(expected), toList(actual))

  @Test
  def testConstant(): Unit = {
    assertEquals(constantList("x")(5), toList(Stream.take(constant("x"))(5)))
    assertEquals(Nil(), toList(Stream.take(constant("x"))(0)))
  }

  private def constantList[A](elem: A)(n: Int): List[A] =
    if(n>0) append(Cons(elem, Nil()), constantList[A](elem)(n-1)) else Nil()

}

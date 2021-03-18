package ulab03

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test
import u03.Streams.Stream
import u03.Streams.Stream.{empty, toList}
import ulab03.TaskStream.drop

class TaskStreamTest {
  val STREAM_SIZE = 10
  val s: Stream[Int] = Stream.take(Stream.iterate(0)(_+1))(STREAM_SIZE)

  @Test
  def testDrop(): Unit = {
    assertEqualsStreams(Stream.take(Stream.iterate(6)(_+1))(4), drop(s)(6))
    assertEqualsStreams(empty(), drop(s)(STREAM_SIZE))
    assertEqualsStreams(s, drop(s)(0))
  }

  private def assertEqualsStreams[A](expected: Stream[A], actual: Stream[A]): Unit = {
    assertEquals(toList(expected), toList(actual))
  }
}

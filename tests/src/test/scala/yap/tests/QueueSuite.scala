package yap
package tests

class QueueSuite extends munit.FunSuite {
  test("should create an empty queue") {
    val expected = true
    val obtained = Queue[Int].isEmpty
    assertEquals(expected, obtained)
  }

  test("should enqueue an element into queue") {
    val queue = Queue(1, 2)
    val expected = Queue(inputStack = Stack(3), outputStack = Stack(2, 1))
    val obtained = queue.enqueue(3)
    assertEquals(obtained, expected)
  }

  test("should not dequeue anything from empty queue") {
    val queue = Queue()
    val expected = (Queue(), None)
    val obtained = queue.dequeue
    assertEquals(obtained, expected)
  }

  test("should dequeue last remaining element from queue") {
    val queue = Queue(1)
    val expected = (Queue(), Some(1))
    val obtained = queue.dequeue
    assertEquals(obtained, expected)
  }

  test("should dequeue an element from queue") {
    val queue = Queue(1, 2, 3)
    val expected = (Queue(2, 3), Some(1))
    val obtained = queue.dequeue
    assertEquals(obtained, expected)
  }
}

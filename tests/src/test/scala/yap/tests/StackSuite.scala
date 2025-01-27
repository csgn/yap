package yap
package tests

class StackSuite extends munit.FunSuite {
  test("should create an empty stack") {
    val expected = true
    val obtained = Stack[Int].isEmpty
    assertEquals(expected, obtained)
  }

  test("should push an element into stack") {
    val stack = Stack(1, 2)
    val expected = Stack(1, 2, 3)
    val obtained = stack.push(3)
    assertEquals(obtained, expected)
  }

  test("should not pop anything from empty stack") {
    val stack = Stack()
    val expected = (Stack(), None)
    val obtained = stack.pop
    assertEquals(obtained, expected)
  }

  test("should pop last remaining element from stack") {
    val stack = Stack(1)
    val expected = (Stack(), Some(1))
    val obtained = stack.pop
    assertEquals(obtained, expected)
  }

  test("should pop an element from stack") {
    val stack = Stack(1, 2, 3)
    val expected = (Stack(1, 2), Some(3))
    val obtained = stack.pop
    assertEquals(obtained, expected)
  }

  test("should return last pushed element from stack") {
    val stack = Stack(1, 2, 3)
    val expected = Some(3)
    val obtained = stack.peek
    assertEquals(obtained, expected)
  }
}

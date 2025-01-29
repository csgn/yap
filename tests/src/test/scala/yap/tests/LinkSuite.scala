package yap
package tests

import Link.{Cons, Nil}

class LinkSuite extends munit.FunSuite {
  test("should create an empty link list") {
    val expected = Nil
    val obtained = Link()
    assertEquals(obtained, expected)
  }

  test("should create a linked list which contains an element") {
    val expected = Cons(1, Nil)
    val obtained = Link(1)
    assertEquals(obtained, expected)
  }

  test("should create a linked list which contains bunch of elements") {
    val expected = Cons(1, Cons(2, Cons(3, Nil)))
    val obtained = Link(1, 2, 3)
    assertEquals(obtained, expected)
  }

  test("should map a function over linked list") {
    val expected = Cons(2, Cons(4, Cons(6, Nil)))
    val obtained = Link(1, 2, 3).map(_ * 2)
    assertEquals(obtained, expected)
  }

  test("should flatmap a function over linked list") {
    val expected = Cons(2, Cons(4, Cons(6, Nil)))
    val obtained = Link(1, 2, 3).flatMap(el => Cons(el * 2, Nil))
    assertEquals(obtained, expected)
  }

  test("should append an element into linked list") {
    val expected = Cons(1, Cons(2, Cons(3, Cons(4, Nil))))
    val obtained = Link(1, 2, 3).append(4)
    assertEquals(obtained, expected)
  }

  test("should prepend an element into linked list") {
    val expected = Cons(4, Cons(3, Cons(2, Cons(1, Nil))))
    val obtained = Link(4, 3, 2).append(1)
    assertEquals(obtained, expected)
  }

  test("should prepend bunch of elements into linked list") {
    val expected = Cons(6, Cons(5, Cons(4, Cons(3, Cons(2, Cons(1, Nil))))))
    val obtained = Link(3, 2, 1).prepend(4, 5, 6)
    assertEquals(obtained, expected)
  }

  test("should append bunch of elements into linked list") {
    val expected = Cons(1, Cons(2, Cons(3, Cons(4, Cons(5, Cons(6, Nil))))))
    val obtained = Link(1, 2, 3).append(4, 5, 6)
    assertEquals(obtained, expected)
  }

  test("should return searched node in linked list") {
    val expected = Some(Cons(2, Cons(3, Nil)))
    val obtained = Link(1, 2, 3).search(2)
    assertEquals(obtained, expected)
  }

  test("should return head of linked list") {
    val expected = Some(1)
    val obtained = Link(1, 2, 3).head
    assertEquals(obtained, expected)
  }

  test("should return tail of linked list") {
    val expected = Cons(2, Cons(3, Nil))
    val obtained = Link(1, 2, 3).tail
    assertEquals(obtained, expected)
  }

  test("should return value at given index") {
    val expected = Some(3)
    val obtained = Link(1, 2, 3).at(2)
    assertEquals(obtained, expected)

    val expected2 = None
    val obtained2 = Link(1, 2, 3).at(42)
    assertEquals(obtained2, expected2)
  }

  test("should return index of given value") {
    val expected = Some(1)
    val obtained = Link(1, 2, 3).index(2)
    assertEquals(obtained, expected)

    val expected2 = None
    val obtained2 = Link(1, 2, 3).index(42)
    assertEquals(obtained2, expected2)
  }

  test("should sum up linked list via foldRight") {
    val expected = 6
    val obtained = Link(1, 2, 3).foldRight(0)(_ + _)
    assertEquals(obtained, expected)
  }

  test("should perform given function onto elements within linked list via foldRight") {
    val expected = "abc"
    val obtained = Link("a", "b", "c").foldRight("")((el, acc) => el + acc)
    assertEquals(obtained, expected)
  }

  test("should perform given function onto elements within linked list via foldLeft") {
    val expected = "abc"
    val obtained = Link("a", "b", "c").foldLeft("")((el, acc) => el + acc)
    assertEquals(obtained, expected)
  }

  test("should reverse the linked list") {
    val expected = Link(3, 2, 1)
    val obtained = Link(1, 2, 3).reverse
    assertEquals(obtained, expected)
  }

  test("should remove an element from linked list") {
    val expected = Link(1, 2, 4, 5)
    val obtained = Link(1, 2, 3, 4, 5).remove(3)
    assertEquals(obtained, expected)
  }

  test("should remove an element at index from linked list") {
    val expected = (Link(2, 3, 4, 5), Some(1))
    val obtained = Link(1, 2, 3, 4, 5).removeAt(0)
    assertEquals(obtained, expected)
  }

  test("should combine two linked list") {
    val l1 = Link(1, 2, 3)
    val l2 = Link(4, 5, 6, 7)
    val expected = Link(5, 7, 9, 7)
    val obtained = l1.combine(l2)
    assertEquals(obtained, expected)
  }
}

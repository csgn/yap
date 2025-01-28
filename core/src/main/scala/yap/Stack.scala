package yap

import Link.Nil

sealed case class Stack[+A] private (private[yap] val elements: Link[A])

object Stack {

  /** Example:
    * {{{
    * scala> Stack()
    * res0: Stack[Int] = Stack(Nil)
    * }}}
    */
  def apply[A](): Stack[A] = Stack(Nil)

  /** Example:
    * {{{
    * scala> Stack(1)
    * res0: Stack[Int] = Stack(Cons(1, Nil))
    * }}}
    */
  def apply[A](a: A): Stack[A] = Stack(Link(a))

  /** Example:
      * {{{
      * scala> Stack(1, 2, 3, 4)
      * res0: Stack[Int] = Stack(Cons(4, Cons(3, Cons(2, Cons(1, Nil)))))
      * }}}
      */
  def apply[A](as: A*): Stack[A] = Stack {
    Link[A].prepend(as: _*)
  }

  implicit class StackOps[A](stack: Stack[A]) {

    /** Example:
      * {{{
      * scala> val stack = Stack(1, 2)
      * stack: Stack[Int] = Stack(Cons(2, Cons(1, Nil)))
      *
      * scala> stack.push(3)
      * res0: Stack[Int] = Stack(Cons(3, Cons(2, Cons(1, Nil))))
      * }}}
      */
    def push(a: A): Stack[A] = stack.copy(elements = stack.elements.prepend(a))

    /** Example:
      * {{{
      * scala> val stack = Stack(1, 2, 3)
      * stack: Stack[Int] = Stack(Cons(3, Cons(2, Cons(1, Nil))))
      *
      * scala> stack.pop
      * res0: Stack[Int] = Stack(Cons(2, Cons(1, Nil)))
      * }}}
      */
    def pop: (Stack[A], Option[A]) = {
      if (stack.isEmpty) (stack, None)
      else {
        val (nextElements, poppedValue) = stack.elements.removeAt(0)
        (stack.copy(elements = nextElements), poppedValue)
      }
    }

    /** Example:
      * {{{
      * scala> val stack = Stack(1, 2, 3)
      * stack: Stack[Int] = Stack(Cons(3, Cons(2, Cons(1, Nil))))
      *
      * scala> stack.peek
      * res0: Option[Int] = Some(3)
      * }}}
      */
    def peek: Option[A] = stack.elements.head

    /** Example:
      * {{{
      * scala> val stack = Stack()
      * stack: Stack[Int] = Stack(Nil)
      *
      * scala> stack.isEmpty
      * res0: Boolean = true
      * }}}
      */
    def isEmpty: Boolean = stack.elements.isEmpty
  }
}

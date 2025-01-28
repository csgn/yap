package yap

sealed case class Queue[+A] private (
  private[yap] val inputStack: Stack[A],
  private[yap] val outputStack: Stack[A]
)

object Queue {

  /** Example:
    * {{{
    * scala> Queue()
    * res0: Queue[Int] = Queue(Stack(Nil), Stack(Nil))
    * }}}
    */
  def apply[A](): Queue[A] = Queue(Stack[A](), Stack[A]())

  /** Example:
    * {{{
    * scala> Queue(1)
    * res0: Queue[Int] = Queue(Stack(Nil), Stack(Cons(1, Nil)))
    * }}}
    */
  def apply[A](a: A): Queue[A] = Queue(
    inputStack = Stack[A](),
    outputStack = Stack[A](a)
  )

  /** Example:
      * {{{
      * scala> Queue(1, 2, 3, 4)
      * res0: Queue[Int] = Queue(Stack(Nil), Stack(Cons(1, Cons(2, Cons(3, Cons(4, Nil))))))
      * }}}
      */
  def apply[A](as: A*): Queue[A] = Queue(
    inputStack = Stack[A](),
    outputStack = Stack[A](elements = Link[A].append(as: _*))
  )

  implicit class QueueOps[A](queue: Queue[A]) {

    /** Example:
      * {{{
      * scala> val queue = Queue(1, 2)
      * queue: Queue[Int] = Queue(Stack(Nil), Stack(Cons(1, Cons(2, Nil))))
      *
      * scala> queue.enqueue(3)
      * res0: Queue[Int] = Queue(Stack(Cons(3, Nil)), Stack(Cons(1, Cons(2, Nil))))
      * }}}
      */
    def enqueue(a: A): Queue[A] = queue.copy(inputStack = queue.inputStack.push(a))

    /** Example:
      * {{{
      * scala> val queue = Queue(1, 2)
      * queue: Queue[Int] = Queue(Stack(Nil), Stack(Cons(1, Cons(2, Nil))))
      *
      * scala> queue.enqueue(Some(3))
      * res0: Queue[Int] = Queue(Stack(Cons(3, Nil)), Stack(Cons(1, Cons(2, Nil))))
      * }}}
      */
    def enqueue(a: Option[A]): Queue[A] = a match {
      case None          => queue
      case Some(element) => queue.enqueue(element)
    }

    /** Example:
      * {{{
      * scala> val queue = Queue(1, 2, 3)
      * queue: Queue[Int] = Queue(Nil, Cons(1, Cons(2, Cons(3, Nil))))
      *
      * scala> queue.dequeue
      * res0: (Queue[Int], Option[Int]) = (Queue(Stack(Nil), Stack(Cons(2, Cons(3, Nil)))), Some(1))
      * }}}
      */
    def dequeue: (Queue[A], Option[A]) = {
      if (queue.isEmpty) (queue, None)
      else {
        if (!queue.outputStack.isEmpty) {
          val (nextOutputStack, poppedValue) = queue.outputStack.pop
          (queue.copy(outputStack = nextOutputStack), poppedValue)
        } else {
          @scala.annotation.tailrec
          def loop(inputStack: Stack[A], outputStack: Stack[A]): (Stack[A], Stack[A], Option[A]) = {
            if (inputStack.isEmpty) {
              val (nextOutputStack, poppedValue) = outputStack.pop
              (inputStack, nextOutputStack, poppedValue)
            } else {
              val (nextInputStack, poppedValue) = inputStack.pop
              val nextOutputStack = outputStack.push(poppedValue)
              loop(nextInputStack, nextOutputStack)
            }
          }

          val (nextInputStack, nextOutputStack, poppedValue) = loop(queue.inputStack, queue.outputStack)
          (queue.copy(inputStack = nextInputStack, outputStack = nextOutputStack), poppedValue)
        }
      }
    }

    /** Example:
      * {{{
      * scala> val queue = Queue()
      * queue: Queue[Int] = Queue(Stack(Nil), Stack(Nil))
      *
      * scala> queue.isEmpty
      * res0: Boolean = true
      * }}}
      */
    def isEmpty: Boolean = queue.inputStack.isEmpty && queue.outputStack.isEmpty
  }
}

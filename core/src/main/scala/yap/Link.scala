package yap

import cats.kernel.Monoid

trait Link[+A]

object Link {
  case object Nil extends Link[Nothing]
  case class Cons[+A](head: A, tail: Link[A] = Nil) extends Link[A]

  /** Example:
    * {{{
    * scala> Link()
    * res0: Link[Int] = Nil
    * }}}
    */
  def apply[A](): Link[A] = Nil

  /** Example:
    * {{{
    * scala> Link(1)
    * res0: Link[Int] = Cons(1, Nil)
    * }}}
    */
  def apply[A](a: A): Link[A] = Cons(a, Nil)

  /** Example:
    * {{{
    * scala> Link(1, 2, 3, 4)
    * res0: Link[Int] = Cons(1, Cons(2, Cons(3, Cons(4, Nil))))
    * }}}
    */
  def apply[A](as: A*): Link[A] = {
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }

  implicit def numericLinkMonoid[T: Numeric]: Monoid[Link[T]] = new Monoid[Link[T]] {
    def empty: Link[T] = Link[T]
    def combine(l1: Link[T], l2: Link[T]): Link[T] = (l1, l2) match {
      case (Nil, Nil)                  => Nil
      case (Cons(l1Head, l1Tail), Nil) => Cons(l1Head, combine(l1Tail, Nil))
      case (Nil, Cons(l2Head, l2Tail)) => Cons(l2Head, combine(Nil, l2Tail))
      case (Cons(l1Head, l1Tail), Cons(l2Head, l2Tail)) =>
        Cons(Numeric[T].plus(l1Head, l2Head), combine(l1Tail, l2Tail))
    }
  }

  implicit def stringLinkMonoid: Monoid[Link[String]] = new Monoid[Link[String]] {
    def empty: Link[String] = Link[String]
    def combine(l1: Link[String], l2: Link[String]): Link[String] = (l1, l2) match {
      case (Nil, Nil)                  => Nil
      case (Cons(l1Head, l1Tail), Nil) => Cons(l1Head, combine(l1Tail, Nil))
      case (Nil, Cons(l2Head, l2Tail)) => Cons(l2Head, combine(Nil, l2Tail))
      case (Cons(l1Head, l1Tail), Cons(l2Head, l2Tail)) =>
        Cons(l1Head + l2Head, combine(l1Tail, l2Tail))
    }
  }

  implicit class LinkOps[A](l1: Link[A]) {

    /** Example:
      * {{{
      * scala> val ll = Link(1, 2, 3)
      * ll: Link[Int] = Cons(1, Cons(2, Cons(3, Nil)))
      *
      * scala> ll.map(el => el * 2)
      * res0: Link[Int] = Cons(2, Cons(4, Cons(6, Nil)))
      * }}}
      */
    def map[B](f: A => B): Link[B] = l1 match {
      case Nil              => Nil
      case Cons(head, tail) => Cons(f(head), tail.map(f))
    }

    /** Example:
      * {{{
      * scala> val ll = Link(1, 2, 3)
      * ll: Link[Int] = Cons(1, Cons(2, Cons(3, Nil)))
      *
      * scala> ll.flatMap(el => Cons(el * 2, Nil))
      * res0: Link[Int] = Cons(2, Cons(4, Cons(6, Nil)))
      * }}}
      */
    def flatMap[B](f: A => Link[B]): Link[B] = l1 match {
      case Nil              => Nil
      case Cons(head, tail) => f(head).append(tail.flatMap(f))
    }

    /** Example:
      * {{{
      * scala> val ll = Link(1)
      * ll: Link[Int] = Cons(1, Nil)
      *
      * scala> ll.prepend(2)
      * res0: Link[Int] = Cons(2, Cons(1, Nil))
      *
      * scala> res0.prepend(3)
      * res1: Link[Int] = Cons(3, Cons(2, Cons(1, Nil)))
      * }}}
      */
    def prepend(a: A): Link[A] = l1 match {
      case Nil              => Cons(a, Nil)
      case Cons(head, tail) => Cons(a, tail.prepend(head))
    }

    /** Example:
      * {{{
      * scala> val ll = Link(1)
      * ll: Link[Int] = Cons(1, Nil)
      *
      * scala> ll.prepend(2, 3, 4)
      * res0: Link[Int] = Cons(4, Cons(3, Cons(2, Cons(1, Nil))))
      * }}}
      */
    @scala.annotation.tailrec
    final def prepend(as: A*): Link[A] = {
      if (as.isEmpty) l1
      else prepend(as.head).prepend(as.tail: _*)
    }

    /** Example:
      * {{{
      * scala> val ll = Link(1)
      * ll: Link[Int] = Cons(1, Nil)
      *
      * scala> ll.append(2)
      * res0: Link[Int] = Cons(1, Cons(2, Nil))
      *
      * scala> res0.append(3)
      * res1: Link[Int] = Cons(1, Cons(2, Cons(3, Nil)))
      * }}}
      */
    def append(a: A): Link[A] = l1 match {
      case Nil              => Cons(a, Nil)
      case Cons(head, tail) => Cons(head, tail.append(a))
    }

    /** Example:
      * {{{
      * scala> val ll = Link(1)
      * ll: Link[Int] = Cons(1, Nil)
      *
      * scala> ll.append(2, 3, 4)
      * res0: Link[Int] = Cons(1, Cons(2, Cons(3, Cons(4, Nil))))
      * }}}
      */
    @scala.annotation.tailrec
    final def append(as: A*): Link[A] = {
      if (as.isEmpty) l1
      else append(as.head).append(as.tail: _*)
    }

    /** Example:
      * {{{
      * scala> val ll = Link(1, 2, 3)
      * ll: Link[Int] = Cons(1, Cons(2, Cons(3, Nil)))
      *
      * scala> val ll2 = Link(4, 5)
      * ll2: Link[Int] = Cons(4, Cons(5, Nil))
      *
      * scala> ll.append(ll2)
      * res0: Link[Int] = Cons(1, Cons(2, Cons(3, Cons(4, Cons(5, Nil)))))
      */
    def append(l2: Link[A]): Link[A] = l1 match {
      case Nil              => l2
      case Cons(head, tail) => Cons(head, tail.append(l2))
    }

    /** Example:
      * {{{
      * scala> val ll = Link(1, 2, 3)
      * ll: Link[Int] = Cons(1, Cons(2, Cons(3)))
      *
      * scala> ll.search(2)
      * res0: Link[Int] = Cons(1, Cons(2, Cons(3, Cons(4, Nil))))
      * }}}
      */
    def search(target: A): Option[Link[A]] = l1 match {
      case Nil => None
      case cons @ Cons(head, tail) => {
        if (head == target) Some(cons)
        else tail.search(target)
      }
    }

    /** Example:
      * {{{
      * scala> val ll = Link(1, 2, 3)
      * ll: Link[Int] = Cons(1, Cons(2, Cons(3)))
      *
      * scala> ll.head
      * res0: Option[Int] = Some(1)
      * }}}
      */
    def head: Option[A] = l1 match {
      case Nil           => None
      case Cons(head, _) => Some(head)
    }

    /** Example:
      * {{{
      * scala> val ll = Link(1, 2, 3)
      * ll: Link[Int] = Cons(1, Cons(2, Cons(3)))
      *
      * scala> ll.tail
      * res0: Option[Link[Int]] = Some(Cons(2, Cons(3, Nil)))
      * }}}
      */
    def tail: Link[A] = l1 match {
      case Nil           => Nil
      case Cons(_, tail) => tail
    }

    /** Example:
      * {{{
      * scala> val ll = Link(1, 2, 3)
      * ll: Link[Int] = Cons(1, Cons(2, Cons(3, Nil)))
      *
      * scala> ll.at(0)
      * res0: Option[Int] = Some(1)
      *
      * scala> ll.at(1)
      * res0: Option[Int] = Some(2)
      *
      * scala> ll.at(42)
      * res0: Option[Int] = None
      * }}}
      */
    def at(index: Int): Option[A] = {
      require(index >= 0, "Index value should be positive integer")

      @scala.annotation.tailrec
      def loop(count: Int, ll: Link[A]): Option[A] = ll match {
        case Nil => None
        case Cons(head, tail) => {
          if (count == index) Some(head)
          else loop(count + 1, tail)
        }
      }

      loop(0, l1)
    }

    /** Example:
      * {{{
      * scala> val ll = Link(1, 2, 3)
      * ll: Link[Int] = Cons(1, Cons(2, Cons(3, Nil)))
      *
      * scala> ll.index(0)
      * res0: Option[Int] = Some(1)
      *
      * scala> ll.index(3)
      * res0: Option[Int] = Some(2)
      *
      * scala> ll.index(42)
      * res0: Option[Int] = None
      * }}}
      */
    def index(target: A): Option[Int] = {
      @scala.annotation.tailrec
      def loop(acc: Int, ll: Link[A]): Option[Int] = ll match {
        case Nil => None
        case Cons(head, tail) => {
          if (head == target) Some(acc)
          else loop(acc + 1, tail)
        }
      }

      loop(0, l1)
    }

    /** Example:
      * {{{
      * scala> val ll = Link("a", "b", "c")
      * ll: Link[String] = Cons("a", Cons("b", Cons("c", Nil)))
      *
      * scala> ll.foldRight("")(_ + _)
      * res0: String = "abc"
      * }}}
      */
    def foldRight[B](acc: B)(f: (A, B) => B): B = l1 match {
      case Nil              => acc
      case Cons(head, tail) => f(head, tail.foldRight(acc)(f))
    }

    /** Example:
      * {{{
      * scala> val ll = Link(1, 2, 3)
      * ll: Link[Int] = Cons(1, Cons(2, Cons(3, Nil)))
      *
      * scala> ll.foldLeft("")(_ + _)
      * res0: String = "abc"
      * }}}
      */
    @scala.annotation.tailrec
    final def foldLeft[B](acc: B)(f: (B, A) => B): B = l1 match {
      case Nil              => acc
      case Cons(head, tail) => tail.foldLeft(f(acc, head))(f)
    }

    /** Example:
      * {{{
      * scala> val ll = Link(1, 2, 3)
      * ll: Link[Int] = Cons(1, Cons(2, Cons(3, Nil)))
      *
      * scala> ll.reverse
      * res0: Link[Int] = Cons(3, Cons(2, Cons(1, Nil)))
      * }}}
      */
    def reverse: Link[A] = {
      l1.foldLeft(Link[A]())((acc, a) => Cons(a, acc))
    }

    /** Example:
      * {{{
      * scala> val ll = Link(1, 2, 3)
      * ll: Link[Int] = Cons(1, Cons(2, Cons(3, Nil)))
      *
      * scala> ll.remove(2)
      * res0: Link[A] = Cons(1, Cons(3, Nil))
      *
      * scala> ll.remove(42)
      * res0: Link[A] = Cons(1, Cons(2, Cons(3, Nil)))
      * }}}
      */
    def remove(target: A): Link[A] = {
      l1.foldLeft(Link[A]())((acc, a) => {
        if (target == a) acc
        else acc.append(a)
      })
    }

    /** Example:
      * {{{
      * scala> val ll = Link(1, 2, 3)
      * ll: Link[Int] = Cons(1, Cons(2, Cons(3, Nil)))
      *
      * scala> ll.removeAt(0)
      * res0: Link[A] = Cons(2, Cons(3, Nil))
      * }}}
      */
    def removeAt(index: Int): (Link[A], Option[A]) = {
      require(index >= 0, "Index value should be positive integer")
      l1.foldLeft(((Link[A](), Option.empty[A])), 0) {
        case (((link, removedValue), count), el) => {
          if (index == count) ((link, Some(el)), count + 1)
          else ((link.append(el), removedValue), count + 1)
        }
      }._1
    }

    /** Example:
      * {{{
      * scala> val ll = Link(1, 2, 3)
      * ll: Link[Int] = Cons(1, Cons(2, Cons(3, Nil)))
      *
      * scala> ll.isEmpty
      * res0: Boolean = false 
      *
      * scala> val ll = Link()
      * ll: Link[A] = Nil
      *
      * scala> ll.isEmpty
      * res1: Boolean = true
      * }}}
      */
    def isEmpty: Boolean = l1 match {
      case Cons(_, _) => false
      case Nil        => true
    }

    /** Example:
      * {{{
      * scala> val l1 = Link(1, 2, 3)
      * l1: Link[Int] = Cons(1, Cons(2, Cons(3, Nil)))
      *
      * scala> val l2 = Link(4, 5, 6, 7)
      * l2: Link[Int] = Cons(4, Cons(5, Cons(6, Cons(7, Nil))))
      *
      * scala> l1.combine(l2)(intLinkMonoid)
      * res0: Link[A] = Cons(5, Cons(7, Cons(9, Cons(7, Nil))))
      * }}}
      */
    def combine(l2: Link[A])(implicit F: Monoid[Link[A]]): Link[A] = {
      F.combine(l1, l2)
    }
  }
}

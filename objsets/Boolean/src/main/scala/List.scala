/**
  * Created by mirceanedelcu on 10/5/16.
  */

package week4

trait List[T] {

  def isEmpty: Boolean
  def head: T
  def tail: List[T]
}

class Cons[T](val head: T, val tail: List[T]) extends List[T] {
  def isEmpty = false
}

class Nil[T] extends List[T] {
  def isEmpty: Boolean = true
  def head: Nothing = throw new NoSuchElementException("Nil.head")
  def tail: Nothing = throw new NoSuchElementException("Nil.tail")
}

object List {

  def apply[T](): List[T] = new Nil[T]

  def apply[T](first: T): List[T] = {
    new Cons(first, new Nil[T])
  }

  def apply[T](first: T, second: T): List[T] = {
    new Cons(first, new Cons(second, new Nil[T]))
  }

  def isort(xs: List[Int]) : List[Int] = xs match {
    case List() => List()
    case y::ys => insert(y, isort(ys))
  }

  def insert(x: Int, xs: List[Int]) : List[Int] = xs match  {
    case List() => List(x)
    case y::ys => if x < y new Cons(x, List(y, ys))
                  else Cons(y, insert(x, ys))
  }
}





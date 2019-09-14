package datastructures

trait Option[+A] {
  def map[B](f: A => B): Option[B] = {
    this match {
      case Some(x) => Some(f(x))
      case None    => None
    }
  }
  def flatMap[B](f: A => Option[B]): Option[B] = {
    map(f).getOrElse(None)
  }

  def getOrElse[B >: A](default: => B): B = {
    this match {
      case Some(x) => x
      case None    => default
    }
  }
  def orElse[B >: A](ob: => Option[B]): Option[B] = {
    map(Some(_)).getOrElse(ob)
  }
  def filter(f: A => Boolean): Option[A] = {
    flatMap { a =>
      if (f(a)) Some(a)
      else None
    }
  }
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    a.flatMap { aa =>
      b.map { bb =>
        f(aa, bb)
      }
    }
  }

}

object Option {

  def sequence[A](as: List[Option[A]]): Option[List[A]] = {
    as match {
      case Nil => Some(Nil)
      case Cons(h, t) =>
        h.flatMap { hh =>
          sequence(t).map { Cons(hh, _) }
        }
    }
  }

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
    a match {
      case Nil => Some(Nil: List[B])
      case Cons(h, t) => {
        f(h).flatMap { hh =>
          traverse(t)(f).map(Cons(hh, _))
        }
      }
    }
  }
}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

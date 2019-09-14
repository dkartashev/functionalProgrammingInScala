package datastructures

case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B] = {
    this match {
      case Right(x) => Right(f(x))
      case Left(e) => Left(e)
    }
  }
  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = {
    this match {
      case Right(x) => f(x)
      case Left(e) => Left(e)
    }
  }
  def flatMap2[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = {
    map(f) match {
      case Right(x) => x
      case Left(e) => Left(e)
    }
  }
  def orElse[EE >: E,B >: A](b: => Either[EE, B]): Either[EE, B] = {
    map(Right(_)) match {
      case Right(x) => x
      case Left(_) => b
    }
  }
  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C):
    Either[EE, C] = {
    this.flatMap { aa =>
      b.map { bb =>
        f(aa, bb)
      }
    }
  }
  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = {
    es match {
      case Nil => Right(Nil)
      case Cons(h, t) => {
        sequence(t).flatMap { li =>
          h.map(Cons(_, li))
        }
      }
    }
  }
  def traverse[E, A, B](as: List[A])(
    f: A => Either[E, B]): Either[E, List[B]] = {
    as match {
      case Nil => Right(Nil)
      case Cons(h, t) => {
        traverse(t)(f).flatMap { li =>
          f(h).map(Cons(_, li))
        }
      }
    }
  }

  def sequence2[E, A](es: List[Either[E, A]]): Either[E, List[A]] = {
    traverse(es)(identity)
  }
}
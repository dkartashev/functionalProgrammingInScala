import Stream.{unfold, _}

trait Stream[+A] {
  def toList: List[A] = {
    this match {
      case Empty => List.empty
      case Cons(h, t) => List(h()) ++ t().toList
    }
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
    case Cons(h, _) if n == 1 => cons(h(), empty)
    case _ => empty
  }

//  def takeWhile(f: A => Boolean): List[A] = {
//    this match {
//      case Empty => List.empty
//      case Cons(h, t) =>
//        if (f(h())) List(h()) ++ t().takeWhile(f)
//        else List.empty
//    }
//  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  def forAll(p: A => Boolean): Boolean = {
    foldRight(true)((a, b) => p(a) && b)
  }

  def forAll2(p: A => Boolean): Boolean = {
    this match {
      case Empty => true
      case Cons(h, t) =>
        if (!p(h())) false
        else t().forAll(p)
    }
  }

  def takeWhile2(f: A => Boolean): Stream[A] = {
    foldRight(empty[A])((a, b) => {
      if (f(a)) cons(a, b)
      else empty[A]
    })
  }

  def headOption(): Option[A] = {
    foldRight(None: Option[A])((a, _) =>
      Some(a)
    )
  }

  def map[B](f: A => B): Stream[B] = {
    foldRight(empty[B])((a, b) =>
      cons(f(a), b)
    )
  }

  def filter(f: A => Boolean): Stream[A] = {
    foldRight(empty[A])((a, b) =>
      if (!f(a)) b
      else cons(a, b)
    )
  }

  def append[B>:A](z: => Stream[B]): Stream[B] = {
    foldRight(z)((a, b) => cons(a, b))
  }

  def flatMap[B](f: A => Stream[B]): Stream[B] = {
    foldRight(empty[B])((a, b) => f(a) append b)
  }

  def exists(p: A => Boolean): Boolean = foldRight(false)((a, b) => p(a) || b)

  def mapViaUnfold[B](f: A => B): Stream[B] = {
    unfold(this){
      case Cons(h, t) =>
        Some((f(h()), t()))
      case Empty => None
    }
  }

  def takeViaUnfold(m: Int): Stream[A] = {
    unfold((this, m)){
      case (Cons(h, t), n) =>
        if (n == 0) None
        else Some(h(), (t(), n - 1))
      case _ => None
    }
  }

  def takeWhile(f: A => Boolean): Stream[A] = {
    unfold(this){
      case Cons(h, t) =>
        if (f(h())) Some(h(), t())
        else None
      case _ => None
    }
  }

//  def zipWith[B, C](s2: Stream[B])(f: (A, B) => C): Stream[C] = {
//    unfold()
//  }
  //zipWith
  //def zipAll[B](s2: Stream[B]): Stream[(Option[A],Option[B])]

}
  case object Empty extends Stream[Nothing]
  case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

  object Stream {
    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
      lazy val head = hd
      lazy val tail = tl
      Cons(() => head, () => tail)
    }
    def empty[A]: Stream[A] = Empty
    def apply[A](as: A*): Stream[A] =
      if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

    def ones: Stream[Int] = cons(1, ones)

    def constant[A](a: A): Stream[A] = {
      cons(a, constant(a))
    }

    def from(n: Int): Stream[Int] = {
      cons(n, from(n + 1))
    }

    def fibs(): Stream[Int] = {
      fibsInner(-1, 1)
    }

    def fibsInner(a: Int, b: Int): Stream[Int] = {
      cons(a + b, fibsInner(b, a + b))
    }

    def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
      f(z) match {
        case None => empty[A]
        case Some((a, s)) => cons(a, unfold(s)(f))
      }
    }

    //Write fibs, from, constant, and ones in terms of unfold.
    def ones1: Stream[Int] = {
      unfold(1)(_ => Some((1, 1)))
    }

    def from1(n: Int): Stream[Int] = {
      unfold(n - 1)(n => Some((n+1, n+1)))
    }

    def constant1(n: Int): Stream[Int] = {
      unfold(n)(_ => Some((n, n)))
    }

    def fibs1: Stream[Int] = {
     unfold((-1, 1)){case (a, b) => Some(a + b, (b, a + b))}
    }

    def main(args: Array[String]): Unit = {
      println(fibs.takeViaUnfold(10).toList)
      println(fibs1.take(10).toList)
    }
  }

package monad

import org.scalacheck.{Gen, Prop}

trait Monoid[A] {
  def op(a1: A, a2: A): A
  def zero: A
}

object Main {
  val stringMonoid = new Monoid[String] {
    override def op(a1: String, a2: String): String = a1 + a2

    override def zero: String = ""
  }

  val intAddition = new Monoid[Int] {
    override def op(a1: Int, a2: Int): Int = a1 + a2

    override def zero: Int = 0
  }

  val intMultiplication = new Monoid[Int] {
    override def op(a1: Int, a2: Int): Int = a1 * a2

    override def zero: Int = 1
  }

  val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2

    override def zero: Boolean = false
  }

  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2

    override def zero: Boolean = true
  }

  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    override def op(a1: Option[A], a2: Option[A]): Option[A] = a1.orElse(a2)

    override def zero: Option[A] = None
  }

  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    override def op(a1: A => A, a2: A => A): A => A = {
      a: A => a2(a1(a))
    }

    override def zero: A => A = {
      a: A => a
    }
  }

  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop = {
    Prop.forAll(gen) { sample =>
      m.op(m.zero, sample) == sample && m.op(sample, m.zero) == sample
    }
  }

  def checkMonoidLaws(): Unit = {
    monoidLaws[Int](intAddition, Gen.posNum[Int]).check()
    monoidLaws[Int](intMultiplication, Gen.posNum[Int]).check()
    monoidLaws[Boolean](booleanOr, Gen.oneOf(false, true)).check()
    monoidLaws[Boolean](booleanAnd, Gen.oneOf(false, true)).check()
  }

  def foldMap[A,B](as: List[A], m: Monoid[B])(f: A => B): B = {
    as.map(f).foldLeft(m.zero)(m.op)
  }

  def foldRight[A, B](as: List[A], z: B)(op: (A, B) => B): B = {
    foldMap(as, endoMonoid[B])(op.curried)(z)
  }

  def foldMapV[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B = {
    if (v.isEmpty) {
      m.zero
    } else {
      val mid = v.length / 2
      if (mid != 0) {
        val (left, right) = v.splitAt(mid)
        m.op(foldMapV(left, m)(f), foldMapV(right, m)(f))
      } else {
        m.op(f(v.head), m.zero)
      }
    }
  }

  def main(args: Array[String]): Unit = {
    val words = List("Hic", "Est", "Index")
    val sRight = words.foldRight(stringMonoid.zero)(stringMonoid.op)
    val sLeft = words.foldLeft(stringMonoid.zero)(stringMonoid.op)
    println(sRight)
    println(sLeft)
  }


}

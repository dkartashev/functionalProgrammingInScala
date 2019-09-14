package datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
object Tree {

  def main(args: Array[String]): Unit = {
    val tr = Branch(Branch(Branch(Leaf(17), Leaf(6)), Leaf(12)), Leaf(14))
    println(map3(tr)(_.toString + "text"))
  }

  def size[A](tr: Tree[A]): Int = {
    tr match {
      case _: Leaf[A] => 1
      case Branch(left, right) => 1 + size(left) + size(right)
    }
  }

  def maximum(tr: Tree[Int]): Int = {
    tr match {
      case Leaf(x) => x
      case Branch(left, right) => maximum(left) max maximum(right)
    }
  }

  def depth[A](tr: Tree[A]): Int = {
    tr match {
      case _: Leaf[A] => 1
      case Branch(left, right) => 1 + (depth(left) max depth(right))
    }
  }

  def map[A, B](tr: Tree[A])(f: A => B): Tree[B] = {
    tr match {
      case l: Leaf[A] => Leaf(f(l.value))
      case Branch(left, right) => Branch[B](map(left)(f), map(right)(f))
    }
  }

  def fold[A, B](tr: Tree[A], z: B)(f:(B, B, B) => B): B = {
    tr match {
      case _ : Leaf[A] => z
      case Branch(left, right) => f(z, fold(left, z)(f), fold(right, z)(f))
    }
  }

  def size2[A](tr: Tree[A]): Int = {
    fold[A, Int](tr, 1)((z, resL, resR) => z + resL + resR)
  }

  def fold2[A, B](tr: Tree[A])(f: A => B)(g: (B, B) => B): B = {
    tr match {
      case Leaf(l) => f(l)
      case Branch(l, r) => g(fold2(l)(f)(g),fold2(r)(f)(g))
    }
  }

  def size3[A](tr: Tree[A]): Int = {
    fold2[A, Int](tr)(_ => 1)(1 + _ + _)
  }

  def maximum3(tr: Tree[Int]): Int = {
    fold2[Int, Int](tr)(a => a)(_ max _)
  }

  def depth3[A](tr: Tree[A]): Int = {
    fold2[A, Int](tr)(_ => 1)(_ max _)
  }

  def map3[A, B](tr: Tree[A])(f: A => B): Tree[B] = {
    fold2[A, Tree[B]](tr)(a => Leaf(f(a)))((a, b) => Branch(a, b))
  }
}

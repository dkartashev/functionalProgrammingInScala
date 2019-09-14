package datastructures

import scala.annotation.tailrec

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil         => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil          => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs)  => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def tail[A](list: List[A]): List[A] = {
    list match {
      case Nil         => Nil
      case Cons(_, xs) => xs
    }
  }

  def setHead[A](head: A, list: List[A]): List[A] = {
    list match {
      case Nil         => Cons(head, Nil)
      case Cons(_, xs) => Cons(head, xs)
    }
  }

  @tailrec
  def drop[A](l: List[A], n: Int): List[A] = {
    if (n == 0) l
    else {
      l match {
        case Nil           => Nil
        case Cons(_, tail) => drop(tail, n - 1)
      }
    }
  }

  @tailrec
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
    l match {
      case Nil => Nil
      case list @ Cons(head, tail) => {
        if (f(head)) dropWhile(tail, f)
        else list
      }
    }
  }

  def init[A](l: List[A]): List[A] = {
    l match {
      case Cons(_, Nil)  => Nil
      case Cons(x, tail) => Cons(x, init(tail))
    }
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil         => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  def length[A](as: List[A]): Int = {
    foldRight[A, Int](as, 0)((_, b) => 1 + b)
  }

  def main(args: Array[String]): Unit = {
    println(hasSubsequence(List(1,2,3,4), List(2,1)))
  }

  @tailrec
  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil        => z
    case Cons(h, t) => foldLeft(t, f(z, h))(f)
  }

  def sumL(l: List[Int]) = {
    foldLeft(l, 0)(_ + _)
  }

  def productL(l: List[Int]) = {
    foldLeft(l, 1)(_ * _)
  }

  def lengthL[A](l: List[A]) = {
    foldLeft(l, 0)((b, _) => b + 1)
  }

  def reverse[A](l: List[A]): List[A] = {
    foldLeft(l, Nil: List[A])((b, a) => Cons(a, b))
  }

  def foldRight2[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
    foldLeft(as, z)((zz, as) => f(as, zz))
  }

  def append[A](headList: List[A], tailList: List[A]): List[A] = {
    foldRight(headList, tailList) { (a, list) =>
      Cons(a, list)
    }
  }

  def flatten[A](listOfLists: List[List[A]]): List[A] = {
    reverse {
      listOfLists match {
        case Nil => Nil
        case Cons(head, tailListOfLists) => {
          foldLeft(tailListOfLists, reverse(head)) { (head, list) =>
            foldLeft(list, head) { (head, el) =>
              Cons(el, head)
            }
          }
        }
      }
    }
  }

  def addOne(list: List[Int]): List[Int] = {
    foldRight(list, Nil: List[Int]) { (el, z) =>
      Cons(el + 1, z)
    }
  }

  def convertToString(l: List[Int]): List[String] = {
    foldRight(l, Nil: List[String]) { (l, z) =>
      Cons(l.toString, z)
    }
  }

  def map[A, B](l: List[A])(f: A => B): List[B] = {
    foldRight(l, Nil: List[B]) { (l, z) =>
      Cons(f(l), z)
    }
  }

  def filter[A](l: List[A])(f: A => Boolean): List[A] = {
    foldRight(l, Nil: List[A]) { (l, z) =>
      if (!f(l)) z
      else Cons(l, z)
    }
  }

  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = {
    flatten{
      map(as)(f)
    }
  }

  def filter2[A](l: List[A])(f: A => Boolean): List[A] = {
    flatMap(l) { a =>
      if (f(a)) List(a)
      else Nil: List[A]
    }
  }

  def zipPlus(l1: List[Int], l2: List[Int]): List[Int] = {
    (l1, l2) match { case (Cons(h1, t1), Cons(h2, t2)) =>
      Cons(h1+h2, zipPlus(t1, t2))
    case (Nil, Nil) => Nil
    }
  }

  def zipWith[A](l1 : List[A], l2: List[A])(f: (A, A) => A): List[A] = {
    (l1, l2) match { case (Cons(h1, t1), Cons(h2, t2)) =>
      Cons(f(h1,h2), zipWith(t1, t2)(f))
    case (Nil, Nil) => Nil
    }
  }

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    hasSubsequenceRec(sup, sub, sub)
  }

  def hasSubsequenceRec[A](sup: List[A], sub: List[A], subFull: List[A]): Boolean = {
    (sup, sub) match {
      case (_, Nil) => true
      case (Nil, _) => false
      case (Cons(h1, t1), Cons(h2, t2)) => {
        if (h1 == h2) hasSubsequenceRec(t1, t2, subFull)
        else hasSubsequenceRec(t1, subFull, subFull)
      }
    }
  }

}

package state

class RNGGenerator {

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (n, rng2) = rng.nextInt
    if (n < 0) (-n, rng2)
    else {
      if (n == Int.MinValue) {
        this.nonNegativeInt(rng2)
      } else {
        (n, rng2)
      }
    }
  }

//  def map[A,B](s: Rand[A])(f: A => B): Rand[B] = rng => {
//    val (a, rng2) = s(rng)
//    (f(a), rng2)
//  }

  def double(rng: RNG): (Double, RNG) = {
    val denum = Int.MaxValue.toDouble
    val (n, rng2) = rng.nextInt
    val d = n.toDouble / denum
    if (d == 1) {
      double(rng2)
    } else {
      (d, rng2)
    }
  }

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (n, rng2) = rng.nextInt
    val (d, rng3) = double(rng2)
    ((n, d), rng3)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val (d, rng2) = double(rng)
    val (n, rng3) = rng2.nextInt
    ((d, n), rng3)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1, rng2) = double(rng)
    val (d2, rng3) = double(rng2)
    val (d3, rng4) = double(rng3)
    ((d1, d2, d3), rng4)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    go(count, List(), rng)
  }

  private def go(left: Int, li: List[Int], rng: RNG): (List[Int], RNG) = {
    if (left == 0) {
      (li, rng)
    } else {
      val (next, rng2) = rng.nextInt
      val liRes = li++List(next)
      go(left - 1, liRes, rng2)
    }
  }

}

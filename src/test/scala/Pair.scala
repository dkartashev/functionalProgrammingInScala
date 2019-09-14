import java.time.format.TextStyle

class Pair[T, S] (val first: T, val second: S){
  def swap(t : T, s : S): Pair[S, T] = {
    new Pair(s, t)
  }

  def swap(pair: Pair[T, S]): Pair[S, T] ={
    new Pair(pair.second, pair.first)
  }
}

class Pair2[T](var first: T, var second: T){
  def swap(): Unit ={
    val tmp = first
    first = second
    second = tmp
  }
}

class Middle[T, C](implicit ev: T => Iterable[C]){
  def middle(t: T): C = {
    val mid = t.size/2
    t.toIterator.drop(mid).next()
  }
}

object Middle extends Middle[String, Char]{
  def main(args: Array[String]): Unit = {
    println(middle("test"))
  }
}
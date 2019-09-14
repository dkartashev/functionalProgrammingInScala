object Sorted {
  def main(args: Array[String]): Unit = {

  }

  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
    if (as.length > 1) {
      if (ordered(as(1), as(2))){
        isSorted(as.drop(1), ordered)
      } else {
        false
      }
    } else {
      true
    }
  }
}

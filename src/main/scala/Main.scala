object Main{
  def pascal(c: Int, r: Int): Int = {
    if(c == 0 || r == 0|| c == r) 1
    else pascal(c-1, r-1) + pascal(c, r-1)
  }

  def balance(chars : List[Char]): Boolean = {
    def isBalanced(chars: List[Char], numberOfOpens: Int): Boolean = {
      if(chars.isEmpty) numberOfOpens == 0
      else if(chars.head == '(') isBalanced(chars.tail, numberOfOpens+1)
      else if(chars.head == ')') {
        if(numberOfOpens > 0) {
          isBalanced(chars.tail, numberOfOpens-1)
        }else{
          false
        }
      } else {
        isBalanced(chars.tail, numberOfOpens)
      }
    }
    isBalanced(chars, 0)
  }


  //или используем текущую монетку или отбрасываем
  //если текущая монетка больше суммы которую осталось разменять, то
  //в обязательном порядке переходим к следующей
  //и не забыть про случай, когда монетка последняя
//  def change(coins : List[Int], limit: Int): Int = {
//    val coin = coins.head
//    if(coin > limit)
//    change(coins.tail, limit) + change(coins, limit - coin)
//
//  }

  def main(args : Array[String]): Unit = {
    println(balance("())".toCharArray().toList))
  }
}


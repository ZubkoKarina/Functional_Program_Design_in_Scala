package example

object Lists {
  def sum(xs: List[Int]): Int = {
    if (xs.isEmpty) 0 
    else xs.head + sum(xs.tail)
  }
  def max(xs: List[Int]): Int = {
    def maxHelper(acc: Int, rest: List[Int]): Int = {
      if (rest.isEmpty) acc
      else {
        val current = rest.head
        val newAcc = if (current > acc) current else acc
        maxHelper(newAcc, rest.tail)
      }
    }
    if (xs.isEmpty) throw new NoSuchElementException("empty list")
    else maxHelper(xs.head, xs.tail)
  }
}
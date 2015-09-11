package hackerrank.thegridsearch

object Solution {

  val myBasis = 1 << 16
  val myFactor = 11

  def main(args: Array[String]): Unit = {
    val is = sys.props.get("local") map io.Source.fromFile getOrElse io.Source.fromInputStream(System.in)
    val input = is.getLines()
    val tests = input.next().toInt

    val result = for (t <- 1 to tests) yield {
      val tg = readTable(input)
      val tp = readTable(input)
      if (recSearch(tg, tp)) "YES" else "NO"
    }

    result foreach println
  }

  def recSearch(big: Seq[String], small: Seq[String], where: Option[Int] = None): Boolean = {
    if (big.size < small.size) false
    else if (small.isEmpty) true
    else where match {
      case None =>
        val qq = small.head.r.findAllIn(big.head).matchData
        if (qq.exists(m => recSearch(big.tail, small.tail, Some(m.start)))) true
        else recSearch(big.tail, small)
      case Some(ww) =>
        big.head.slice(ww, ww + small.head.length) == small.head && recSearch(big.tail, small.tail, where)
    }
  }

  def readTable(input: Iterator[String]) = {
    val List(r, c) = input.next().split(" ").map(_.toInt).toList
    val dd = for (rr <- 1 to r) yield input.next()
    dd.toVector
  }
}
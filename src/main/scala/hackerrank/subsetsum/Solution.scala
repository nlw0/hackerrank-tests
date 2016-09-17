package hackerrank.subsetsum

object Solution {

  def main(args: Array[String]) {
    import collection.Searching._

    val is = sys.props.get("local") map io.Source.fromFile getOrElse io.Source.fromInputStream(System.in)
    val input = is.getLines()
    val n = input.next().toInt
    val a = input.next().split(" ").take(n).map(_.toLong)
    util.Sorting.quickSort(a)

    val dd = a.reverse.tail.scanLeft(a.last)(_ + _).toVector

    println(dd)

    val t = input.next().toInt
    for (i <- 1 to t) {
      val s = input.next().toLong
      val result = dd search s match {
        case Found(xx) if xx < dd.size =>
          xx + 1
        case InsertionPoint(xx) if xx < dd.size =>
          xx + 1
        case xx =>
          -1
      }
      println(result)
    }
  }
}

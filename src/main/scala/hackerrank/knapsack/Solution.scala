package hackerrank.knapsack

object Solution {

  def main(args: Array[String]): Unit = {
    val is = sys.props.get("local") map io.Source.fromFile getOrElse io.Source.fromInputStream(System.in)
    val input: Iterator[String] = is.getLines()

    val t = input.next().toInt

    def kdata = for (i <- 1 to t) yield readVector(input)

    def result = kdata map { case (k, d) => solve(k, d) }

    result foreach println
  }

  @annotation.tailrec
  def solve(k: Int, data: Seq[Long], possible: Set[Long] = Set(0L)): Long = {
    if (data.isEmpty || (possible contains k)) possible.max
    else {
      val dd +: ds = data
      if (possible contains dd) solve(k, ds, possible)
      else {
        def newPossible = possible flatMap (_ to k by dd)
        solve(k, ds, newPossible)
      }
    }
  }

  def readVector(input: Iterator[String]) = {
    val Array(n, k) = input.next().split(" ").take(2).map(_.toInt)
    (k, input.next().split(" ").take(n).map(_.toLong).toVector)
  }
}
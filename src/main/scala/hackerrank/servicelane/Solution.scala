package hackerrank.servicelane

object Solution {

  def main(args: Array[String]): Unit = {
    val is = sys.props.get("local") map io.Source.fromFile getOrElse io.Source.fromInputStream(System.in)
    val input = is.getLines()
    val Array(n, t) = input.next().split(" ").map(_.toInt)

    val lane = input.next().split(" ").take(n).map(_.toInt).toVector

    val result = for (i <- 1 to t) yield {
      val Array(i, j) = input.next().split(" ").map(_.toInt)
      lane.slice(i, j + 1).min
    }

    result foreach println
  }

}

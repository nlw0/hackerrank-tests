package hackerrank.maxsubarray

object Solution {

  def main(args: Array[String]): Unit = {
    val is = sys.props.get("local") map io.Source.fromFile getOrElse io.Source.fromInputStream(System.in)
    val input: Iterator[String] = is.getLines()

    val t = input.next().toInt

    def result = for (i <- 1 to t) yield solveCase(readInput(input))

    def fmt = result map { case (a, b) => s"$a $b" }

    fmt foreach println
  }

  def readInput(input: Iterator[String]) = {
    val n = input.next().toInt
    input.next().split(" ").take(n).map(_.toLong)
  }

  def solveCase(data: Seq[Long]) = {
    val result = data.tail.foldLeft((data.head, data.head, data.head)) {
      case ((maxHere, maxGeneral, nonContiguous), x) =>
        val newNc = if (nonContiguous < 0) x max nonContiguous
        else if (x > 0) nonContiguous + x else nonContiguous

        val newMaxHere = x max (maxHere + x)

        val newMaxGeneral = newMaxHere max maxGeneral

        (newMaxHere, newMaxGeneral, newNc)
    }
    (result._2, result._3)
  }
}
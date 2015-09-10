package hackerrank.alternatingcharacters

object Solution {
  def main(args: Array[String]): Unit = {
    val is = sys.props.get("local") map io.Source.fromFile getOrElse io.Source.fromInputStream(System.in)
    val input = is.getLines()
    val n = input.next().toInt

    def processed = for {
      i <- 1 to n
      row = input.next()
    } yield processLine(row)

    processed foreach println
  }

  def processLine(ss: String): Int =
    ss.sliding(2, 1) map { tt => if (tt(0) == tt(1)) 1 else 0 } sum
}


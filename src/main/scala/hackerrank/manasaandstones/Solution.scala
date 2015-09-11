package hackerrank.manasaandstones

object Solution {

  def main(args: Array[String]): Unit = {
    val is = sys.props.get("local") map io.Source.fromFile getOrElse io.Source.fromInputStream(System.in)
    val input = is.getLines()
    val tests = readInt(input)

    val result = for (t <- 1 to tests) yield {
      val (n, ai, bi) = readCase(input)
      val (a, b) = (ai min bi, ai max bi)
      val inc = b - a
      val ends = if (b != a)
        (1 to n - 1).foldLeft(List(a * (n - 1)))({ case (acc, _) => acc.head + inc :: acc })
      else
        List(a * (n - 1))
      ends.reverse.mkString(" ")
    }

    result foreach println
  }

  def readCase(input: Iterator[String]) =
    (readInt(input), readInt(input), readInt(input))

  def readInt(input: Iterator[String]) = input.next().toInt
}
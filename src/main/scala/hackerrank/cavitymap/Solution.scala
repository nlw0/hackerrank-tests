package hackerrank.cavitymap

object Solution {

  def main(args: Array[String]): Unit = {
    val is = sys.props.get("local") map io.Source.fromFile getOrElse io.Source.fromInputStream(System.in)
    val input: Iterator[String] = is.getLines()
    val data = readInput(input)

    val result = if (data.length>2)
      data.take(1) ++ (data.sliding(3, 1) map (findMax(_).mkString)) ++ data.takeRight(1)
    else
      data
    result foreach println
  }

  def findMax(abc: Seq[String]) =
    abc match {
      case Seq(a, b, c) =>
        for (i <- b.indices)
          yield if ((i > 0) && i < (b.length - 1) &&
                    Vector(a(i), c(i), b(i - 1), b(i + 1)).forall(_ < b(i))) 'X'
          else b(i)
      case _ =>
        abc
    }

  def readInput(input: Iterator[String]) = {
    val n = input.next().toInt
    for (i <- 1 to n) yield input.next()
  }
}

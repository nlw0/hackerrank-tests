package hackerrank.taumandbday

object Solution {

  def main(args: Array[String]): Unit = {
    val is = sys.props.get("local") map io.Source.fromFile getOrElse io.Source.fromInputStream(System.in)
    val input: Iterator[String] = is.getLines()
    val t = readInt(input)
    val data = for (i <- (1 to t).iterator) yield readInput(input)

    val result = data map calculatePrice

    result foreach println
  }

  def calculatePrice(xx: Seq[Long]) = {
    val Seq(b, w, x, y, z) = xx
    if (y + z < x)
      b * (y + z) + w * y
    else if (x + z < y)
      b * x + w * (x + z)
    else
      b * x + w * y
  }

  def readInput(input: Iterator[String]) = readVector(input, 2) ++ readVector(input, 3)

  def readVector(input: Iterator[String], size: Int) = input.next().split(" ").take(size).map(_.toLong).toList

  def readInt(input: Iterator[String]) = input.next().toInt
}

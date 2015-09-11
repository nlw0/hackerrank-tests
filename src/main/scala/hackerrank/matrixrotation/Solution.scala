package hackerrank.matrixrotation

object Solution {

  def main(args: Array[String]): Unit = {
    val is = sys.props.get("local") map io.Source.fromFile getOrElse io.Source.fromInputStream(System.in)
    val input = is.getLines()

    val (r, n, m, data) = readInput(input)

    genRot(n, m, data, r) foreach println
    // genTest(n, m) foreach println
  }

  def genRot(n: Int, m: Int, data: Seq[Seq[Int]], r: Int) = {
    for (i <- 0 until n) yield (
      for (j <- 0 until m)
        yield {
          val (g, h) = ringCoords(n, m, i, j)
          val ringsize = 2 * (n - 2*g - 1 + m - 2*g - 1)
          val (ii, jj) = cartesian(n, m, g, (h + r) % ringsize)
          data(ii)(jj)
        }
      ).mkString(" ")
  }

  def genTest(n: Int, m: Int) = {
    for (i <- 0 until n) yield (
      for (j <- 0 until m)
        yield {
          // 'a' - 1 + ringCoords(n, m, i, j)._2
          val (g, h) = ringCoords(n, m, i, j)
          'a' - 1 + cartesian(n, m, g, h)._1
        }
      ).map(_.toChar).mkString
  }

  def sillyIndex(n: Int, m: Int, i: Int, j: Int) = {
    i * m + j
  }

  def ringCoords(n: Int, m: Int, i: Int, j: Int) = {
    val g = (if (j < m / 2) j else m - j - 1) min (if (i < n / 2) i else n - i - 1)

    val h = if (i == g) j - g
    else if (j == m - 1 - g) (m - 2 * g) + (i - g - 1)
    else if (i == n - 1 - g) (m - 2 * g) + (n - 2 * g - 1) + (m - 1 - 1 - g - j)
    else if (j == g) (m - 2 * g) + (n - 2 * g - 1) + (m - 1 - 1 - 2 * g) + (n - 1 - g - i)
    else -1

    (g, h)
  }

  def cartesian(n: Int, m: Int, g: Int, h: Int) = {
    if (h < m - 2 * g) (g, h + g)
    else if (h < m - 2 * g + n - 2 * g - 2) (h - (m - 3 * g) + 1, m - 1 - g)
    else if (h < m - 2 * g + n - 2 * g + m - 2 * g - 2) (n - 1 - g, m - 1 - (h - (m - 2 * g + n - 3 * g - 2)))
    else (n - (h - (m - 2 * g + n - 2 * g + m - 2 * g - 2)) - 2 - g, g)
  }

  def readInput(input: Iterator[String]) = {
    val Vector(n, m, r) = readVector(input, 3)
    (r, n, m, readMatrix(input, n, m))
  }

  def readInt(input: Iterator[String]) = input.next().toInt

  def readMatrix(input: Iterator[String], n: Int, m: Int) = for (_ <- 1 to n) yield readVector(input, m)

  def readVector(input: Iterator[String], n: Int) = input.next().split(" ").take(n).map(_.toInt).toVector
}
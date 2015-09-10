/*
package hackerrank.flowers

object Solution {

  def main(args: Array[String]): Unit = {
    val is = sys.props.get("local") map io.Source.fromFile getOrElse io.Source.fromInputStream(System.in)
    val input: Iterator[String] = is.getLines()
    val data = readInput(input)

    val result = data.take(1) ++ (data.sliding(3, 1) map findmax) ++ data.takeRight(1)
    result foreach println
  }

  def findmax(abc: Seq[String]) = {
    val Seq(a, b, c) = abc
    for (i <- b.indices)
      yield if (i > 0 && i < b.length - 1 && Vector(a(i), c(i), b(i - 1), b(i + 1)).forall(_ < b(i)) 'X'
      else b(i)
  }

  def readInput(input: Iterator[String]) = {
    val n = readIn(input)
    for (_ <- 1 to n) yield readVector(input, n)
  }

  def readVector(input: Iterator[String], size: Int) =
    input.next().split(" ").take(size).map(_.toInt).toVector

  def readInt(input: Iterator[String]) = input.next().toInt
}
*/

package hackerrank.pairs

object Solution {

  def main(args: Array[String]): Unit = {
    val is = sys.props.get("local") map io.Source.fromFile getOrElse io.Source.fromInputStream(System.in)
    val input: Iterator[String] = is.getLines()
    val (k, data) = readInput(input)

    println(data.iterator.count(data contains _ + k))
  }

  def readInput(input: Iterator[String]) = {
    val List(n, k) = readVector(input, 2).toList
    (k, readVector(input, n))
  }

  def readVector(input: Iterator[String], size: Int) = input.next().split(" ").iterator.take(size).map(_.toInt).toSet
}

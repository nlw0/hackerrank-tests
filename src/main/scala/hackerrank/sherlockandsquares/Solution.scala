package hackerrank.sherlockandsquares

object Solution {

  def main(args: Array[String]): Unit = {
    val is = sys.props.get("local") map io.Source.fromFile getOrElse io.Source.fromInputStream(System.in)
    val input = is.getLines()
    val n = input.next().toInt
    val data = (1 to n) map { _ => input.next().split(" ").map(_.toInt).toList }

    val result = data map f
    result foreach println
  }

  def f(data: Seq[Int]): Int = {
    val Seq(a, b) = data
    nsquares(b) - nsquares(a - 1)
  }

  val squares = for (n <- 1 to 40000) yield n * n

  def nsquares(n: Int): Int = squares.count(_ <= n)
}

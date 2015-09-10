package hackerrank.finddigts

object Solution {

  def main(args: Array[String]): Unit = {
    val is = sys.props.get("local") map io.Source.fromFile getOrElse io.Source.fromInputStream(System.in)
    val input = is.getLines()
    val n = input.next().toInt
    val data = (1 to n) map { _ => input.next().toInt }

    val result = data map f
    result foreach println
  }

  def f(n: Int): Int = digits(n).foldLeft(0) { (acc, d) => if (d == 0 || n % d != 0) acc else acc + 1 }

  def digits(n: Int): List[Int] = if (n == 0) Nil else (n % 10) :: digits(n / 10)

}

package hackerrank.lonelyinteger

object Solution {

  def main(args: Array[String]): Unit = {
    val is = sys.props.get("local") map io.Source.fromFile getOrElse io.Source.fromInputStream(System.in)
    val input = is.getLines()
    val n = input.next().toInt
    val data = input.next().split(" ").take(n).map(_.toInt).toList

    val result = data.tail.foldLeft(data.head) {
      (a,b) => a ^b }
    println(result)
  }
}

package hackerrank.longestcommonsubsequence

object Solution {

  def main(args: Array[String]): Unit = {
    val is = sys.props.get("local") map io.Source.fromFile getOrElse io.Source.fromInputStream(System.in)
    val input: Iterator[String] = is.getLines()
    val (xx, yy) = readInput(input)

    println(lcs(xx, yy).mkString(" "))
  }

  var memo = Map[(List[Int], List[Int]), List[Int]]()

  def lcs(sa: List[Int], sb: List[Int]): List[Int] = {
    if (memo.contains((sa, sb))) memo((sa, sb))
    else {
      val result = if (sa.isEmpty || sb.isEmpty) Nil
      else if (sa.head == sb.head) {
        sa.head :: lcs(sa.tail, sb.tail)
      } else {
        val aa = lcs(sa.tail, sb)
        val bb = lcs(sa, sb.tail)
        if (aa.size > bb.size) aa else bb
      }
      memo += (sa, sb) -> result
      result
    }

  }

  def readInput(input: Iterator[String]) = {
    val List(na, nb) = readVector(input, 2)
    (readVector(input, na), readVector(input, nb))
  }

  def readVector(input: Iterator[String], size: Int) =
    input.next().split(" ").take(size).map(_.toInt).toList
}

package hackerrank.stringcompression

object Solution {
  def main(args: Array[String]): Unit = {
    val is = sys.props.get("local") map io.Source.fromFile getOrElse io.Source.fromInputStream(System.in)
    val input = is.getLines()
    val row = input.next()

    for (el <- processLine(row)) print(el)
    print("\n")
  }

  def processLine(ss: String): Stream[String] =
    if (ss.isEmpty) Stream()
    else {
      val nextChar = ss.head
      val n = ss.takeWhile(_ == nextChar).length
      (if (n == 1) s"$nextChar" else s"$nextChar$n") #:: processLine(ss.drop(n))
    }
}


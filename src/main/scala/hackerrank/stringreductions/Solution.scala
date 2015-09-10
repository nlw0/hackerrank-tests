package hackerrank.stringreductions

object Solution {
  def main(args: Array[String]): Unit = {
    val is = sys.props.get("local") map io.Source.fromFile getOrElse io.Source.fromInputStream(System.in)
    val input = is.getLines()
    val row = input.next()

    println(processLine(row))
  }

  def processLine(ss: String): String = {
    val (outs, _) = ss.foldLeft((List.empty[Char], Set.empty[Char])) {
      case ((acc, log), ch) =>
        if (log contains ch) (acc, log) else (ch :: acc, log + ch)
    }
    outs.reverse.mkString
  }
}


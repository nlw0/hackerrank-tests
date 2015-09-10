package hackerrank.filterelements

object Solution {
  def main(args: Array[String]): Unit = {
    val is = sys.props.get("local") map io.Source.fromFile getOrElse io.Source.fromInputStream(System.in)
    val input = is.getLines()
    val t = input.next().toInt

    def results = for (i <- 1 to t) yield {
      val Array(n, k) = input.next().split(" ").map(_.toInt)
      val data = readIntVector(input, n)
      val result = processData(data, k)
      if (result.isEmpty) -1 else result.mkString(" ")
    }

    results foreach println
  }

  def processData(data: Seq[Int], k: Int) = {
    val toRemove = data.groupBy(identity).mapValues(_.size).filter({ case (_, v) => v < k }).keySet

    val (acc, _) = data.foldLeft((List[Int](), Set[Int]())) {
      case ((acc, log), n) => if (log.contains(n) || toRemove.contains(n)) (acc, log) else (n :: acc, log + n)
    }
    acc.reverse
  }

  def readIntVector(input: Iterator[String], n: Int) = input.next().split(" ").take(n).map(_.toInt).toVector
}

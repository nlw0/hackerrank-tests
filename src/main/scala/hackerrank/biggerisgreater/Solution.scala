package hackerrank.biggerisgreater

object Solution {

  def main(args: Array[String]): Unit = {
    val is = sys.props.get("local") map io.Source.fromFile getOrElse io.Source.fromInputStream(System.in)
    val input = is.getLines()

    val data = readInput(input)

    val result = data map process map (_.getOrElse("no answer"))

    result foreach println
  }

  def process(ss: String) = {
    val prefix = dropIncreasingPrefix(ss.reverse).reverse
    if (prefix.isEmpty) None
    else {
      val suffix = ss.drop(prefix.length)

      val pivot = prefix.last

      val toSwap = suffix.reverse.find(_ > pivot)

      val newPrefix = prefix.dropRight(1) ++ toSwap

      val newSuffix = ((suffix diff toSwap.toList) ++ List(pivot)).sorted

      // Some((prefix, pivot, suffix, newPrefix, newSuffix))
      Some(newPrefix ++ newSuffix)
    }
  }

  def dropIncreasingPrefix(ss: String): String =
    if (ss.length > 1 && ss.head <= ss(1))
      dropIncreasingPrefix(ss.tail)
    else
      ss.tail


  def readInput(input: Iterator[String]) = {
    val n = readInt(input)
    readStrings(input, n)
  }

  def readInt(input: Iterator[String]) = input.next().toInt

  def readStrings(input: Iterator[String], n: Int) = for (_ <- 1 to n) yield input.next()
}
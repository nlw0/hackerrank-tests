package hackerrank.icecreamparlor

object Solution {

  def main(args: Array[String]): Unit = {
    val is = sys.props.get("local") map io.Source.fromFile getOrElse io.Source.fromInputStream(System.in)
    val input: Iterator[String] = is.getLines()
    val data = readInput(input)

    val result = for ((m, pp) <- data) yield f(m, pp)

    result foreach { case List(a, b) => println(s"$a $b") }
  }

  def f(m: Int, pp: Seq[Int]) = {
    val dd = pp.zipWithIndex.groupBy(_._1).mapValues(_ map (1 + _._2))

    def result = for {
      a <- dd.keys
      b = m - a
      if b != a
      if pp contains b
    } yield List(dd(a)(0), dd(b)(0)).sorted

    val res1 = result.headOption

    res1.getOrElse(List(dd(m / 2)(0), dd(m / 2)(1))).sorted
  }

  def readInput(input: Iterator[String]) = {
    val t = readInt(input)
    for (_ <- 1 to t) yield readCase(input)
  }

  def readCase(input: Iterator[String]) = {
    val m = readInt(input)
    val n = readInt(input)
    (m, readVector(input, n))
  }

  def readInt(input: Iterator[String]) = input.next().toInt

  def readVector(input: Iterator[String], size: Int) =
    input.next().split(" ").take(size).map(_.toInt)
}

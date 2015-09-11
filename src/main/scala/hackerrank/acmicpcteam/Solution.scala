package hackerrank.acmicpcteam

object Solution {

  def main(args: Array[String]): Unit = {
    val is = sys.props.get("local") map io.Source.fromFile getOrElse io.Source.fromInputStream(System.in)
    val input = is.getLines()

    val data = readCase(input)

    val nTopics = for {
      i <- data.indices.take(data.length - 1)
      j <- data.indices.drop(i + 1)
    } yield numTopics(data(i), data(j))

    val teamsByTopics = nTopics.groupBy(identity)
    val maxTopics = teamsByTopics.keys.max
    println(maxTopics)
    println(teamsByTopics(maxTopics).size)
  }

  def numTopics(a: Seq[Boolean], b: Seq[Boolean]): Int =
    (a zip b) count { case (aa, bb) => aa || bb }

  def readCase(input: Iterator[String]) = {
    val Seq(n, m) = readVector(input, 2)
    for (i <- 1 to n) yield readBinaryVector(input, m)
  }

  def readVector(input: Iterator[String], n: Int) = input.next().split(" ").take(n).map(_.toInt).toVector

  def readBinaryVector(input: Iterator[String], n: Int) = input.next().take(n).map(_ == '1').toVector
}
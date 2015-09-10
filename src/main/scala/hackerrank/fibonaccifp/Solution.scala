package hackerrank.fibonaccifp

object Solution {
  val basis = 100000007

  def main(args: Array[String]): Unit = {
    val is = sys.props.get("local") map io.Source.fromFile getOrElse io.Source.fromInputStream(System.in)
    val input = is.getLines()
    val t = input.next().toInt

    def results = for (i <- 1 to t) yield {
      val n = input.next().toInt
      fibo(n)
    }

    results foreach println
  }

  val fibo: Stream[Int] = {
    0 #:: 1 #:: (fibo zip fibo.tail).map({ case (a, b) => (a + b) % basis })

  }
}

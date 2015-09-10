package hackerrank.pentagonalnumbers

object Solution {

  def main(args: Array[String]) {
    val is = sys.props.get("local") map io.Source.fromFile getOrElse io.Source.fromInputStream(System.in)
    val input = is.getLines()
    val t = input.next().toInt
    val data = for (i <- 1 to t) yield {
      val n = input.next().toInt
      println(ww(n))
    }
  }

  /*
    val qq: Stream[Int] = 1 #:: (qq zip Stream.continually(3) map ({case (a,b)=> a+b}))
    val ww: Stream[Int] = 0 #:: (ww zip qq map ({case (a,b)=> a+b}))
  */

  def qq(n: Int, a: Int = 1): Int = if (n == 0) a else qq(n - 1, 3 + a)

  @annotation.tailrec
  def ww(n: Int, a: Int = 0): Int = if (n == 0) a else ww(n - 1, a + qq(n - 1))

}
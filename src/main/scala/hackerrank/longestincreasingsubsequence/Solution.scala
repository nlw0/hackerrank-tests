package hackerrank.longestincreasingsubsequence

import scala.annotation.tailrec

object Solution {

  def main(args: Array[String]): Unit = {
    val is = sys.props.get("local") map io.Source.fromFile getOrElse io.Source.fromInputStream(System.in)
    val input = is.getLines()
    val n = input.next().toLong
    val data = for (i <- (1L to n).iterator) yield input.next().toLong

    val result = listConstruct(data, Vector(data.next()))
    println(result)
  }

  implicit def mkOps[A](x: A)(implicit ord: math.Ordering[A]): ord.Ops = ord.mkOrderingOps(x)

  import scala.collection.Searching._

  @tailrec
  def listConstruct(data: Iterator[Long],
                    lis: Vector[Long] = Vector()): Long = {
    // println(lis.length + " - " + lis.reverse.take(20))
    if (!data.hasNext) lis.length
    else {
      val dd = data.next()
      def pp = lis.search(dd).insertionPoint
      def newLis = if (dd > lis.last) lis :+ dd else lis.updated(pp, dd)
      listConstruct(data, newLis)
    }
  }

}

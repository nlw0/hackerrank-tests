package hackerrank.reversefactorization

object Solution {

  case class State(toAdd: List[Long], steps: List[Long])

  def main(args: Array[String]): Unit = {
    val is = sys.props.get("local") map io.Source.fromFile getOrElse io.Source.fromInputStream(System.in)
    val input = is.getLines()
    val Array(n, k) = input.next().split(" ").map(_.toLong)
    val factors = input.next().split(" ").take(k.toInt).map(_.toLong).filter(n % _ == 0).sorted.toList
    //    println(factors)
    val result = bfsConstruct(n, Vector(State(factors, List(1))))
    val resStr = result match {
      case None => "-1"
      case Some(xx) =>
        xx.reverse.mkString(" ")
    }
    println(resStr)
  }

  def bfsConstruct(n: Long, toVisit: Vector[State]): Option[Seq[Long]] = toVisit match {
    case Vector() => None
    case vv +: vs =>
      //      println(vv)
      if (vv.steps.head == n)
        Some(vv.steps)
      else if (vv.steps.head > n)
        bfsConstruct(n, vs)
      else {
        val next = for (i <- vv.toAdd.indices) yield {
          val ff = vv.toAdd.drop(i)
          State(ff, (ff.head * vv.steps.head) :: vv.steps)
        }
        bfsConstruct(n, vs ++ next)
      }
  }

}

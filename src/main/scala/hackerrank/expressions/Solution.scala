package hackerrank.expressions

object Solution {

  trait Ops

  case object Add extends Ops {
    override def toString = "+"
  }

  case object Sub extends Ops {
    override def toString = "-"
  }

  case object Mul extends Ops {
    override def toString = "*"
  }

  def main(args: Array[String]): Unit = {
    val is = sys.props.get("local") map io.Source.fromFile getOrElse io.Source.fromInputStream(System.in)
    val input = is.getLines()
    val n = input.next().toInt
    val data = input.next().split(" ").take(n).map(_.toInt).toList

    val result = dfsExpression(List((data.head, List(Add), data.tail)))

    val intercalate = data.head +: (result zip data.tail).flatMap {
      case (op, b) => List(op, b)
    }

    println(intercalate.mkString)
  }

  val allOps: List[Ops] = List(Mul, Sub, Add)

  val nextOp: Map[Ops, Ops] = Map(Add -> Sub, Sub -> Mul)

  @annotation.tailrec
  def dfsExpression(toVisit: List[(Int, List[Ops], List[Int])]): Seq[Ops] = {
    println(toVisit)
    toVisit.head match {
      case (0, ops, Nil) => ops.reverse
      case (acc, ops, Nil) => dfsExpression(toVisit.tail)
      case (acc, op :: ops, data) =>
        val newAcc = evalSingle(acc, op, data.head)
        val nextStates = List((newAcc, Add :: op :: ops, data.tail)) ++ nextOp.get(op).map(nxOp => (acc, nxOp :: ops, data))
        dfsExpression(nextStates ++ toVisit.tail)
      case _ =>
        List()
    }
  }

  def evalSingle(a: Int, op: Ops, b: Int): Int = op match {
    case Add => (a + b) % 101
    case Sub => (a - b) % 101
    case Mul => (a * b) % 101
  }
}

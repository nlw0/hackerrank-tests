package hackerrank.queensonboard

object Solution {

  sealed trait Node {
    def dom: collection.SortedMap[Int, Set[Int]]

    def inc: Int
  }

  case class NodeDo(dom: collection.SortedMap[Int, Set[Int]], inc: Int) extends Node

  case class NodeMemo(dom: collection.SortedMap[Int, Set[Int]], inc: Int) extends Node

  def main(args: Array[String]): Unit = {
    val is = sys.props.get("local") map io.Source.fromFile getOrElse io.Source.fromInputStream(System.in)
    val input: Iterator[String] = is.getLines()

    val t = input.next().toInt

    def boards = for (i <- (1 to t).iterator) yield readBoard(input)

    def result = boards map { bb =>
      if (bb.isEmpty) 0 else solve(List(NodeDo(bb, bb.firstKey)))
    }

    result foreach println
  }

//  val memo = collection.mutable.HashMap[collection.SortedMap[Int, Set[Int]], Int]()

  val memoInit = Map[Node, Int](
    NodeDo(collection.SortedMap(), 0) -> 0,
    NodeDo(collection.SortedMap(), 1) -> 1)

  @annotation.tailrec
  def solve(stack: List[Node], acc: Int = 0, memo: Map[Node, Int] = memoInit): Int =
    stack match {
      case Nil => acc

      case nn :: st if memo contains nn =>
//        println("yoy")
        solve(st, acc + memo(nn), memo)

      case NodeDo(dom, inc) :: st =>
        val pos = dom.firstKey
        val na = NodeDo(dom.from(pos + 1), 0)
        val nb = NodeDo(na.dom -- dom(pos), 1)
        val newStack = na :: nb :: NodeMemo(dom, acc) :: st
        solve(newStack, acc + inc, memo)

      case NodeMemo(dom, inc) :: st =>
//        println("record" + dom + " -> " + inc)
        solve(st, acc, memo + (NodeDo(dom, inc) -> (acc - inc)))
    }

  def readBoard(input: Iterator[String]) = {
    val Array(n, m) = input.next().split(" ").take(2).map(_.toInt)
    val chr = for (i <- 1 to n) yield input.next().take(m)

    def blocks = for {
      i <- chr.indices
      j <- chr.head.indices
      if chr(i)(j) == '#'
    } yield i * (m + 1) + j

    def domain = for {
      i <- 0 until n
      j <- 0 until m
      if chr(i)(j) == '.'
      pos = i * (m + 1) + j
    } yield pos -> genDomain(blocks.toSet, n, m, pos)

    collection.SortedMap[Int, Set[Int]] (domain: _*)
  }

  def genDomain(blocks: Set[Int], n: Int, m: Int, pos: Int) = {
    val k = m + 1
    def dirs = Array(1, k - 1, k, k + 1)
    dirs.iterator.foldLeft(Set[Int] ()) {
      (dom, dir) => dom ++ genDomDirection(blocks, n, m, pos + dir, dir)
    }
  }

  @annotation.tailrec
  def genDomDirection(blocks: Set[Int],
                      n: Int,
                      m: Int,
                      pos: Int,
                      dir: Int,
                      acc: Set[Int] = Set()
                       ): Set[Int] = {
    def invalid(x: Int) = blocks.contains(x) || !bounds(x)

    def bounds(x: Int) = x >= 0 && x % (m + 1) < m && x < n * (m + 1)

    if (invalid(pos)) acc
    else genDomDirection(blocks, n, m, pos + dir, dir, acc + pos)
  }
}

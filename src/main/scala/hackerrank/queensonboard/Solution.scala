package hackerrank.queensonboard

object Solution {

  sealed trait StackThing

  case class Node(dom: collection.SortedSet[Int], inc: Int) extends StackThing

  case class Memo(nn: Node, acc: Int) extends StackThing

  def main(args: Array[String]): Unit = {
    val is = sys.props.get("local") map io.Source.fromFile getOrElse io.Source.fromInputStream(System.in)
    val input: Iterator[String] = is.getLines()

    val t = input.next().toInt

    def boards = for (i <- (1 to t).iterator) yield readBoard(input)

    def result = boards map { bb =>
      if (bb.isEmpty) 0 else solve(bb, List(Node(bb.keySet, 0)))
    }

    result foreach println
  }

  @annotation.tailrec
  def solve(neigh: collection.SortedMap[Int, Set[Int]],
            stack: List[StackThing],
            acc: Int = 0,
            memo: Map[Node, Int] = Map[Node, Int]()): Int =
    stack match {
      case Nil => acc

      case (nn: Node) :: st if memo contains nn =>
        solve(neigh, st, (acc + memo(nn)) % 1000000007, memo)

      case Node(dom, inc) :: st if dom.isEmpty =>
        solve(neigh, st, (acc + inc) % 1000000007, memo)

      case Node(dom, inc) :: st =>
        val pos = dom.firstKey
        val na = Node(dom.from(pos + 1), 0)
        val nb = Node(na.dom -- neigh(pos), 1)
        val newStack = na :: nb :: Memo(Node(dom, inc), acc) :: st
        solve(neigh, newStack, (acc + inc) % 1000000007, memo)

      case Memo(nn, accIni) :: st =>
        val incTotal = ((acc - accIni) + 1000000007) % 1000000007
        solve(neigh, st, acc, memo + (nn -> incTotal))
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
    // def dirs = Array(-k - 1, -k, -k + 1, -1, 1, k - 1, k, k + 1)
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

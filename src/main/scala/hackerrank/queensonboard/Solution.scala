package hackerrank.queensonboard

object Solution {

  def main(args: Array[String]): Unit = {
    val is = sys.props.get("local") map io.Source.fromFile getOrElse io.Source.fromInputStream(System.in)
    val input: Iterator[String] = is.getLines()

    val t = input.next().toInt

    def boards = for (i <- (1 to t).iterator) yield readBoard(input)

    def result = boards map (bb => solve(bb))

    result foreach println
  }

  val memo = collection.mutable.HashMap[Map[Int, Set[Int]], Int]()

  def solve(dom: Map[Int, Set[Int]]): Int = {
    if (memo contains dom) memo(dom)
    else if (dom.isEmpty) 0
    else {
      val pos = dom.keys.min

      val aa = if (dom contains pos) 1 + solve(dom.filterKeys(_ >= pos + 1) -- dom(pos)) else 0
      val bb = solve(dom.filterKeys(_ >= pos + 1))

      val rr = (aa + bb) % 1000000007
      memo(dom) = rr
      rr
    }
  }

  def readBoard(input: Iterator[String]) = {
    val Array(n, m) = input.next().split(" ").take(2).map(_.toInt)
    val chr = for (i <- 1 to n) yield input.next().take(m)

    def blocks = for {i <- chr.indices
                      j <- chr.head.indices
                      if chr(i)(j) == '#'} yield i * (m + 1) + j

    def domain = for {
      i <- 0 until n
      j <- 0 until m
      if chr(i)(j) == '.'
      pos = i * (m + 1) + j
    } yield pos -> genDomain(blocks.toSet, n, m, pos)

    Map[Int, Set[Int]](domain:_*)
  }

  def genDomain(blocks: Set[Int], n: Int, m: Int, pos: Int) = {
    val k = m + 1
    def dirs = Array(1, k - 1, k, k + 1)
    dirs.iterator.foldLeft(Set[Int]()) {
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

    // def bounds(x: Int) = x >= 0 && x % (n + 1) < n && x < m * (n + 1)
    def bounds(x: Int) = x >= 0 && x % (m + 1) < m && x < n * (m + 1)

    if (invalid(pos)) acc
    else genDomDirection(blocks, n, m, pos + dir, dir, acc + pos)
  }
}

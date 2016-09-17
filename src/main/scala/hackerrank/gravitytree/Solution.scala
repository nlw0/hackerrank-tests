package hackerrank.gravitytree

object Solution {

  case class Node(count: Int, sum: Int, sqsum: Int) {
    def append(newDesc: Node): Node = {
      Node(
        count + newDesc.count + 1,
        sum + newDesc.sum + newDesc.count + 1,
        sqsum + newDesc.force(1)
      )
    }

    def force(b: Int) = {
      sqsum + 2 * b * sum + b * b * (count + 1)
    }
  }


  def main(args: Array[String]): Unit = {
    val is = sys.props.get("local") map io.Source.fromFile getOrElse io.Source.fromInputStream(System.in)
    val input = is.getLines()
    val n = input.next().toInt
    val parent = Array(0, 0) ++ (input.next() split " " map (_.toInt))

    def parents(node: Int, acc: List[Int] = Nil): List[Int] =
      if (parent(node) == 0) node :: acc
      else parents(parent(node), node :: acc)

    val children = (2 to n) map (c => (parent(c), c)) groupBy (_._1) mapValues (_.map(_._2))

    val mem = collection.mutable.Map[Int, Node]()

    def dfsNode(x: Int): Node = if (mem contains x) mem(x) else {
      val out = (Node(0, 0, 0) /: children.getOrElse(x, List[Int]())) {
        case (a, v) => a.append(dfsNode(v))
      }
      mem(x) = out
      out
    }


    def treecomp(pa: List[Int], pb: List[Int]) =
      pa.size + pb.size - 2 * (pa intersect pb).size

    def dist(a: Int, b: Int) = treecomp(parents(a), parents(b))

    def recforce(u: Int, v: Int, d: Int = 0, acc: Int = 0): Int =
      if (u == v) acc + dfsNode(v).force(d)
      else recforce(parent(u), v, d + 1, acc + dfsNode(u).force(d) - dfsNode(u).force(2 + d))

    def force(u: Int, v: Int) = {
      if (parents(u) contains v) {
        recforce(u, v)
      } else {
        dfsNode(v).force(dist(u, v))
      }
    }

    val q = input.next().toInt
    for (i <- 1 to q) {
      val qq = input.next() split " "
      val u = qq(0).toInt
      val v = qq(1).toInt
      println(force(u, v))
    }
  }
}

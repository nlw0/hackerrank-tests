package hackerrank.bitterchocolate

object Solution {

  val boardLimit = 25

  case class Board(a: Int, b: Int, c: Int) {
    def origins = {
      if (a > b && b > c) {
        (for (aa <- a + 1 to boardLimit) yield Board(aa, b, c)) ++
        (for (bb <- b + 1 to a) yield Board(a, bb, c)) ++
        (for (cc <- c + 1 to b) yield Board(a, b, cc))
      } else if (a > b && b == c) {
        (for (aa <- a + 1 to boardLimit) yield Board(aa, b, c)) ++
        (for (bb <- b + 1 to a; cc <- c to bb) yield Board(a, bb, cc))
      } else if (a == b && b > c) {
        (for (aa <- a + 1 to boardLimit; bb <- b to aa) yield Board(aa, bb, c)) ++
        (for (cc <- c + 1 to a) yield Board(a, b, cc))
      } else if (a == b && b == c) {
        for (aa <- a + 1 to boardLimit; bb <- b to aa; cc <- c to bb) yield Board(aa, bb, cc)
      } else for (_ <- 0 until 0) yield Board(-1, -1, -1)
    }

    def destinationRange = a + b + c

    def destinations = {
      Set[Board]() ++
      (for (aa <- 0 to a - 1) yield Board(aa, b min aa, c min aa)) ++
      (for (bb <- 0 to b - 1) yield Board(a, bb, c min bb)) ++
      (for (cc <- 0 to c - 1) yield Board(a, b, cc))
    }

  }

  def main(args: Array[String]): Unit = {
    val is = sys.props.get("local") map io.Source.fromFile getOrElse io.Source.fromInputStream(System.in)
    val input = is.getLines()
    val n = input.next().toInt
    val data = for (i <- 1 to n) yield {
      val Array(a, b, c) = input.next().split(" ").take(3).map(_.toInt)
      Board(a, b, c)
    }

    val toxic = toxicStates()

    val result = data map { board => !(toxic contains board) }
    val gui = result map { b => if (b) "WIN" else "LOSE" }
    gui foreach println
  }

  def toxicStates(toxic: Set[Board] = Set(),
                  newWinning: Set[Board] = Set(Board(0, 0, 0)),
                  winning: Set[Board] = Set(),
                  nontoxicMoves: Map[Board, Set[Board]] = initialMoves
                   ): Set[Board] = {
    if (newWinning.isEmpty) toxic
    else {
      val newNontoxicMoves = newWinning.flatMap(ww => ww.origins map ((ww, _))).foldLeft(nontoxicMoves) {
        case (acc, (ww, oo)) =>
          acc + (oo -> (acc(oo) - ww))
      }

      val newToxic = newNontoxicMoves.filter({ case ((k, v)) => v.isEmpty && !(toxic contains k) }).keys.toSet

      val nextNewWinning = newToxic flatMap (_.origins)

      toxicStates(toxic ++ newToxic, nextNewWinning, winning ++ newWinning, newNontoxicMoves)
    }
  }

  val allBoards = for (a <- 0 to boardLimit; b <- 0 to a; c <- 0 to b) yield Board(a, b, c)
  val initialMoves = allBoards.map({ bb => bb -> bb.destinations }).toMap - Board(0, 0, 0)
}

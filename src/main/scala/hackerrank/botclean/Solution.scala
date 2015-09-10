package hackerrank.botclean

object Solution {
  def main(args: Array[String]) = {
    val pos = Console.readLine
    val board = new Array[String](5)
    for (i <- 0 until 5) {
      board.update(i, Console.readLine)
    }
    nextMove(pos, board)
  }

  def nextMove(pos: String, board: Array[String]) = {
    val List(mi, mj) = pos.split(" ").map(_.toInt).toList
    val List(pi, pj) = findDirt(board).head

    val move = if (pi > mi) "DOWN"
    else if (pi < mi) "UP"
    else if (pj > mj) "RIGHT"
    else if (pj < mj) "LEFT"
    else "CLEAN"

    println(move)
  }

  def findDirt(board: Array[String]) = for {
    i <- board.indices
    j <- if (i % 2 == 0) board(i).indices else board(i).indices.reverse
    if board(i)(j) == 'd'
  } yield List(i, j)

}

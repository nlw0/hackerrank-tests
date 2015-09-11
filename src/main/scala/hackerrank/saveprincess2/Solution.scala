/*
package hackerrank.saveprincess2


object Solution {
  def main(args: Array[String]) = {
    val n = Console.readInt
    var x_y = Console.readLine.split(" ")
    val r = x_y(0).toInt
    val c = x_y(1).toInt
    val grid = new Array[String](n)
    for (i <- 0 until n) {
      grid.update(i, Console.readLine)
    }
    nextMove(n, r, c, grid)
  }

  def nextMove(player: Int, r: Int, c: Int, grid: Array[String]) = {

    val (pi, pj) = findPrincess(grid)
    val (mi, mj) = (r, c)

    if (pi > mi) "DOWN"
    else if (pi < mi) "UP"
    else if (pj > mj) "RIGHT"
    else if (pj < mj) "LEFT"
    else "WTF"
  }

  def findPrincess(grid: Array[String]) = {
    val princessSweep = for {
      i <- grid.indices
      j <- grid.indices
      if grid(i)(j) == 'p'
    } yield (i, j)
    princessSweep.head
  }

}
*/

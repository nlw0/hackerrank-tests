package hackerrank.saveprincess


object Solution {
  def main(args: Array[String]) = {
        val m = Console.readLine.toInt
        val grid = new Array[String](m)
        for (i <- 0 until m) {
          grid.update(i, Console.readLine)
        }
    displayPathtoPrincess(m, grid)
  }

  /* Refer to Output format section for more details */
  def displayPathtoPrincess(m: Int, grid: Array[String]) = {
    val princessSweep = for {
      i <- List(0, m - 1)
      j <- List(0, m - 1)
      if grid(i)(j) == 'p'
    } yield (i, j)

    val meSweep = for {
      i <- 0 to m - 1
      j <- 0 to m - 1
      if grid(i)(j) == 'm'
    } yield (i, j)

    val princess = princessSweep.head
    val me = meSweep.head

    recMove(princess, me)  foreach println
  }

  def recMove(target: (Int, Int), me: (Int, Int)): List[String] = {
    val (pi, pj) = target
    val (mi, mj) = me

    if (pi > mi) "DOWN" :: recMove(target, (mi + 1, mj))
    else if (pi < mi) "UP" :: recMove(target, (mi - 1, mj))
    else if (pj > mj) "RIGHT" :: recMove(target, (mi, mj + 1))
    else if (pj < mj) "LEFT" :: recMove(target, (mi, mj - 1))
    else Nil
  }
}

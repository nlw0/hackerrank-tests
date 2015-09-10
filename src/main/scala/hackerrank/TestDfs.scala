package hackerrank

object TestDfs extends App {
  def integerKnapsackTest(cc: Seq[Int], k: Int)(state: Seq[Int]): Option[Seq[Int]] = {
    val v = ((cc.takeRight(state.size) zip state) map { case (a, b) => a * b }).sum

    if (v == k) Some(state) else None
  }

  def integerKnapsackNext(cc: Seq[Int], k: Int)(state: Seq[Int]): List[Seq[Int]] = {
    val v = ((cc.takeRight(state.size) zip state) map { case (a, b) => a * b }).sum

    if (v >= k) List()
    else if (state.size < cc.size)
      List((state.head + 1) +: state.tail, 1 +: state)
    else
      List((state.head + 1) +: state.tail)
  }

  val k = 11
  val cc = List(15, 13, 2)
  val initial = List(List(1))

  println(MyDFS.run[Seq[Int], Seq[Int]](initial, integerKnapsackNext(cc, k), integerKnapsackTest(cc, k)))
}


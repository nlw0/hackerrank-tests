package hackerrank.sumnim

object Solution {

  case class State(stacks: List[Int], xor: Int) {
    def children = {
      val newXor = xor ^ stacks(0) ^ stacks(1) ^ (stacks(0) + stacks(1))
      State(stacks.drop(1), xor) :: State(stacks(0) + stacks(1) :: stacks.drop(2), newXor) :: Nil
    }
  }


  def main(args: Array[String]): Unit = {
    val is = sys.props.get("local") map io.Source.fromFile getOrElse io.Source.fromInputStream(System.in)
    val input = is.getLines()
    val n = input.next().toInt
    val data = input.next().split(" ").take(n).map(_.toInt).toList

    val ini = State(data, data reduce (_ ^ _))

    val result = dfsNim(ini)

    println(result)
  }

  var memo: Map[State, Long] = Map()

  def dfsNim(st: State): Long = {
    if (memo contains st) memo(st)
    else {
      val result = if (st.stacks.size <= 1) {
        if (st.xor == 0) 1L else 0L
      } else {
        st.children map dfsNim sum
      }
      memo = memo + (st -> result)
      result
    }
  }

}

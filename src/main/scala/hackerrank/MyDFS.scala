package hackerrank

object MyDFS {
  def run[A, B](toVisit: List[A], next: (A => List[A]), test: (A => Option[B])): Option[B] = {
    if (toVisit.isEmpty)
      None
    else {
      val state = toVisit.head
      val tt = test(state)
      if (tt.isEmpty) run(next(state) ++ toVisit.tail, next, test)
      else tt
    }
  }
}


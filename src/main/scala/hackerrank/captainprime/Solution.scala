package hackerrank.captainprime

object Solution {

  def main(args: Array[String]): Unit = {
    val is = sys.props.get("local") map io.Source.fromFile getOrElse io.Source.fromInputStream(System.in)
    val input = is.getLines()
    val n = input.next().toInt
    val data = for (i <- (1 to n).iterator) yield input.next().toInt

    val result = data map classifier

    result foreach println
  }

  def classifier(n: Int) = {
    val tln = trimLeft(n)
    val tl = tln.forall(isPrime)
    val trn = trimRight(n)
    val tr = trn.forall(isPrime)
    // val nozero = !hasZeroDigit(n)
    val noZero = trn.forall(x => (x % 10) > 0)
    (noZero, tl, tr) match {
      case (true, true, true) => "CENTRAL"
      case (true, true, false) => "LEFT"
      case (true, false, true) => "RIGHT"
      case _ => "DEAD"
    }
  }

  def trimRight(n: Int, acc: List[Int] = Nil): Seq[Int] = if (n == 0) acc else trimRight(n / 10, n :: acc)

  def trimLeft(n: Int, acc: List[Int] = Nil): Seq[Int] = if (n == 0) acc else trimLeft(trimOneLeft(n), n :: acc)

  def trimOneLeft(n: Int, out: Int = 1): Int = if (10 * out > n) n % out else trimOneLeft(n, out * 10)

  val limit = 1000

  val primes = {
    val composites = collection.mutable.HashMap[Int, collection.mutable.Set[Int]]()
    val theprimes = collection.mutable.Set[Int]()

    def myIsPrime(n: Int) = !(composites contains n)

    def update(k: Int, v: Int) =
      if (composites contains k) composites(k) += v
      else composites(k) = collection.mutable.Set(v)

    for (n <- 2 until limit)
      if (myIsPrime(n)) {
        theprimes.add(n)
        update(2 * n, n)
      } else
        for (p <- composites.remove(n).get) update(n + p, p)

    theprimes
  }

  def isPrime(n:Int) = {
    (n > 1) && ((primes contains n) || primes.forall(n % _ != 0))
  }
}

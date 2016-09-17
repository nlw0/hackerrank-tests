package hackerrank.hugegcdfp

object Solution {
  val mm = 1000000007

  def factorize(n: Int) = recfactorize(n, primes)

  def recfactorize(n: Int, pp: Seq[Int], ff: List[Int] = Nil): List[Int] =
    if (n == 1) ff
    else if (n % pp.head == 0) recfactorize(n / pp.head, pp, pp.head :: ff)
    else recfactorize(n, pp.tail, ff)

  def main(args: Array[String]): Unit = {
    val is = sys.props.get("local") map io.Source.fromFile getOrElse io.Source.fromInputStream(System.in)
    val input = is.getLines()
    val n = input.next().toInt
    val aa = input.next().split(" ").take(n).map(_.toInt).toList
    val m = input.next().toInt
    val bb = input.next().split(" ").take(m).map(_.toInt).toList

    val fa = aa flatMap factorize
    val fb = bb flatMap factorize
    println((fa intersect fb).foldLeft(1) (_ * _ % mm))
  }

  val limit = 10000

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

    theprimes.toList.sorted
  }

  def isPrime(n: Int) = (n > 1) && ((primes contains n) || primes.forall(n % _ != 0))

}

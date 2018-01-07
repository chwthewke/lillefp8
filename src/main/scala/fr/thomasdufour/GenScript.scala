package fr.thomasdufour

object GenScript {

  import rng.Rng
  val seed = Rng(0xDEADBEEF)

  {
    var rng = seed
    for (i <- 1 to 5) {
      print(s"${rng.seed} ")
      rng = rng.next
    }
    println()
  }

  case class Gen[A](run: Rng => (A, Rng))

  val long: Gen[Long] =
    Gen(r => (r.seed, r.next))

  val int: Gen[Int] =
    Gen(r => (r.seed.toInt, r.next))

  val boolean: Gen[Boolean] =
    Gen(r => (r.seed >= 0, r.next))

  val char: Gen[Char] =
    Gen(r => (r.seed.toChar, r.next))

  val double: Gen[Double] =
    Gen(r => ((r.seed >>> 11) * 1.1102230246251565e-16, r.next))

}

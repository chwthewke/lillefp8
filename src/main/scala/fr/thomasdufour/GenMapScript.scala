package fr.thomasdufour

import fr.thomasdufour.rng.Rng

object GenMapScript {

  val long: Gen[Long] = Gen(r => (r.seed, r.next))

  case class Gen[A](run: Rng => (A, Rng)) {
    def map[B](f: A => B): Gen[B] = Gen { r =>
      val (a, next) = this.run(r)
      (f(a), next)
    }
  }

  val int: Gen[Int] = long.map(_.toInt)
  val boolean: Gen[Boolean] = long.map(_ >= 0)
  val char: Gen[Char] = long.map(_.toChar)
  val double: Gen[Double] = long.map(l => (l >>> 11) * 1.1102230246251565e-16)

  def const[A](a: A): Gen[A] = long.map(_ => a)
  def proba(p: Double): Gen[Boolean] = double.map(_ < p)
  def upTo(n: Int): Gen[Int] = double.map(x => (x * n).toInt)

  def fixedList[A](gen: Gen[A], n: Int): Gen[List[A]] =
    if (n == 0) const(Nil)
    else
      Gen { r =>
        val (head, r1) = gen.run(r)
        val (tail, r2) = fixedList(gen, n - 1).run(r1)
        (head :: tail, r2)
      }

}

package fr.thomasdufour

import fr.thomasdufour.rng.Rng

import scala.util.Random

object GenFlatMapScript {

  val long: Gen[Long] = Gen(r => (r.seed, r.next))

  val int: Gen[Int] = long.map(_.toInt)
  val boolean: Gen[Boolean] = long.map(_ >= 0)
  val char: Gen[Char] = long.map(_.toChar)
  val double: Gen[Double] = long.map(l => (l >>> 11) * 1.1102230246251565e-16)

  def const[A](a: A): Gen[A] = long.map(_ => a)
  def proba(p: Double): Gen[Boolean] = double.map(_ < p)
  def upTo(n: Int): Gen[Int] = double.map(x => (x * n).toInt)

  case class Gen[A](run: Rng => (A, Rng)) {
    def map[B](f: A => B): Gen[B] = Gen { r =>
      val (a, next) = this.run(r)
      (f(a), next)
    }

    def flatMap[B](f: A => Gen[B]): Gen[B] = Gen { r =>
      val (a, next) = this.run(r)
      f(a).run(next)
    }
  }

  def fixedList[A](gen: Gen[A], n: Int): Gen[List[A]] =
    if (n == 0) const(Nil)
    else gen.flatMap(a => fixedList(gen, n - 1).map(as => a :: as))

  def list[A](gen: Gen[A]): Gen[List[A]] =
    for {
      n <- upTo(32768)
      l <- fixedList(gen, n)
    } yield l

  def tuple[A, B](ga: Gen[A], gb: Gen[B]): Gen[(A, B)] =
    ga.flatMap(a => gb.map(b => (a, b)))

  def option[A](ga: Gen[A]): Gen[Option[A]] =
    proba(0.1).flatMap(
      isNone =>
        if (isNone) const(None)
        else ga.map(Some(_)))

  def either[A, B](ga: Gen[A], gb: Gen[B]): Gen[Either[A, B]] =
    boolean.flatMap(
      isLeft =>
        if (isLeft) ga.map(Left(_))
        else gb.map(Right(_)))

  trait Wheel
  trait Engine

  case class Car(engine: Engine, wheels: List[Wheel])

  def car(genWheel: Gen[Wheel], genEngine: Gen[Engine]): Gen[Car] =
    for {
      engine <- genEngine
      wheels <- fixedList(genWheel, 4)
    } yield Car(engine, wheels)

  object Wheel extends Wheel
  object Engine extends Engine

  def genFunction[A, B]: Gen[A => B] = ???

  def constFunction[A, B](gb: Gen[B]): Gen[A => B] =
    gb.map(b => (_ => b))

  def wildFunction[A, B](gb: Gen[B]): Gen[A => B] =
    const(_ => gb.run(Rng(Random.nextLong))._1)

  // Gen[A]   ~ Rng => (A, Rng)
  // CoGen[A] ~ (A, Rng) => Rng

  case class CoGen[A](consume: (A, Rng) => Rng)

  def function[A, B](cogen: CoGen[A], gen: Gen[B]): Gen[A => B] = Gen { r =>
    def f(a: A): B = gen.run(cogen.consume(a, r))._1
    (f, r.next)
  }

  {
    val longCoGen: CoGen[Long] = CoGen { case (a, r) => Rng(r.seed ^ a).next }

    val longToInt: Gen[Long => Int] = function(longCoGen, int)

    val f = longToInt.run(Rng(1234))._1

    f(0L)
    f(1L)
  }

}

package fr.thomasdufour

import cats.kernel.Order
import cats.kernel.laws.discipline.OrderTests
import org.scalacheck.Arbitrary
import org.scalacheck.Gen
import org.scalacheck.Prop._

object DiceGame {

  //import org.scalacheck.Gen._

  case class Dice(h: Int, m: Int, l: Int) {
    require(1 <= l && l <= m && m <= h && h <= 6)
  }

  object Dice {
    val D421 = Dice(4, 2, 1)
    val D111 = Dice(1, 1, 1)
    def aces(n: Int) = Dice(n, 1, 1)
    def same(n: Int) = Dice(n, n, n)
    def sequence(n: Int) = Dice(n, n - 1, n - 2)
    // ...
  }

  def compareDice(left: Dice, right: Dice): Int = ???
  // like Comparable (-1, 0 ou 1)

  val genDice: Gen[Dice] = for {
    a <- Gen.choose(1, 6)
    b <- Gen.choose(1, 6)
    c <- Gen.choose(1, 6)
    (h, l) = (a max b max c, a min b min c)
    m = a + b + c - h - l
  } yield Dice(h, m, l)

  implicit val arbDice: Arbitrary[Dice] = Arbitrary(genDice)

  val prop1 = forAll { (dice1: Dice, dice2: Dice) =>
    val (best, worst) = (??? : (Dice, Dice))
    // TODO: déterminer la meilleure combinaison parmi dice1, dice2

    compareDice(best, worst) >= 0
  }

  val genAces: Gen[Dice] =
    Gen.choose(2, 6).map(Dice.aces)

  val genSame: Gen[Dice] =
    Gen.choose(2, 6).map(Dice.same)

  val prop2 = forAll(genAces, genDice) { (aces: Dice, same: Dice) =>
    compareDice(aces, same) ?= 1
  }

  val propRefl = forAll { (d: Dice) =>
    compareDice(d, d) ?= 0
  }

  val propSym = forAll { (d1: Dice, d2: Dice) =>
    compareDice(d1, d2) == -compareDice(d2, d1)
  }

  val propTrans = forAll { (d1: Dice, d2: Dice, d3: Dice) =>
    (compareDice(d1, d2) >= 0 && compareDice(d2, d3) >= 0) ==>
      compareDice(d1, d3) >= 0
  }

  // Alternative avec cats
  OrderTests(new Order[Dice] {
    override def compare(x: Dice, y: Dice): Int = compareDice(x, y)
  })

}

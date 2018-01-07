package fr.thomasdufour

object IntroScript {

  import org.scalacheck.Prop._

  {
    // Notre première propriété
    val propLengthPositive = forAll { s: String =>
      s.length >= 0
    }

    propLengthPositive.check

  }

  {
    // Une loi
    val propAssoc = forAll { (s: Int, t: Int, r: Int) =>
      (s + t) + r ?= s + (t + r)
    }

    propAssoc.check
  }

  { //
    val propAbs = forAll { x: Int =>
      math.abs(x) >= 0
    }

    propAbs.check
  }

  {
    //
    val propAbs = forAll { x: Int =>
      x > Int.MinValue ==>
        math.abs(x) >= 0
    }

    propAbs.check
  }

  {
    import math.sqrt

    // 1er essai
    {
      val propSqrtMono = forAll { (x: Double, y: Double) =>
        x < y ==>
          math.sqrt(x) < math.sqrt(y)
      }
    }

    // 2e essai
    {
      val propSqrtMono = forAll { (x: Double, y: Double) =>
        (x < y && x >= 0 && y >= 0) ==>
          math.sqrt(x) < math.sqrt(y)
      }
    }

    // 3e essai
    {
      import org.scalacheck.Gen

      val posDouble = Gen.choose(0d, Double.MaxValue)

      val propSqrtMono = forAll(posDouble, posDouble) {
        (x: Double, y: Double) =>
          x < y ==>
            math.sqrt(x) < math.sqrt(y)
      }
    }

    {
      def sort(xs: List[Int]): List[Int] = xs match {
        case Nil           => Nil
        case x :: Nil      => x :: Nil
        case x :: y :: Nil => (x min y) :: (x max y) :: Nil
        case _             => xs // Trop compliqué
      }

      val propSort = forAll { xs: List[Int] =>
        sort(xs) ?= xs.sorted
      }
    }

  }
}

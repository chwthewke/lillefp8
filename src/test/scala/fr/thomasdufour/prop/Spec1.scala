package fr.thomasdufour.prop

import org.scalacheck.Properties
import org.scalacheck.Prop.forAll

class Spec1 extends Properties("Introduction") {

  property("length >= 0") = forAll { s: String =>
    s.length >= 0
  }

  property("associativity") = forAll { (s: String, t: String, r: String) =>
    (s + t) + r == s + (t + r)
  }

  property("toLower after toUpper") = forAll { s: String =>
    s.toLowerCase == s.toUpperCase.toLowerCase
  }

}

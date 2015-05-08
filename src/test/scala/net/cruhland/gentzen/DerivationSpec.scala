package net.cruhland.gentzen

import org.scalatest._
import prop._

import org.scalacheck._

class DerivationSpec extends GentzenSpec {

  property("a single formula variable matches any simple formula") {
    forAll { (formula: Formula) =>
      assertResult(None) {
        Derivation.matches(Atom(FormulaVariable("A")), formula)
      }
    }
  }

  property("a constant matches itself") {
    assertResult(None) {
      val constant = Atom(Constant.buildWithoutValidation("0"))
      Derivation.matches(constant, constant)
    }
  }

  property("a constant matches nothing other than itself") {
    forAll { (formula: Formula) =>
      val constant = Atom(Constant.buildWithoutValidation("0"))
      whenever(formula != constant) {
        assertResult(Some(ConstantMismatch("0", formula))) {
          Derivation.matches(constant, formula)
        }
      }
    }
  }

}

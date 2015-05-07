package net.cruhland.gentzen

import org.scalatest._
import prop._

import org.scalacheck._

class DerivationSpec extends GentzenSpec {

  property("a single formula variable matches any formula") {
    forAll { (formula: Formula[String]) =>
      assertResult(None) {
        Derivation.matches(Atom.buildSchema(FormulaVariable("A")), formula)
      }
    }
  }

  property("a constant matches itself") {
    assertResult(None) {
      val template = Atom.buildSchema(Constant("0"))
      Derivation.matches(template, Atom.buildWithoutValidation("0"))
    }
  }

  property("a constant matches nothing other than itself") {
    forAll { (formula: Formula[String]) =>
      whenever(formula != Atom.buildWithoutValidation("0")) {
        assertResult(Some(ConstantMismatch("0", formula))) {
          Derivation.matches(Atom.buildSchema(Constant("0")), formula)
        }
      }
    }
  }

}

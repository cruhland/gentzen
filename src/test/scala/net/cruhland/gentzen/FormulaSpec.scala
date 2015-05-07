package net.cruhland.gentzen

import org.scalatest._
import prop._
import org.scalacheck._
import Arbitrary.arbitrary
import Shrink.shrink

class FormulaSpec extends GentzenSpec {

  import FormulaSpec._

  property("[Atom.build()] empty name") {
    assertResult(None) {
      Atom.build("")
    }
  }

  property("[Atom.build()] names with invalid chars") {
    forAll(genStringContainingAnyOf(InvalidNameChars)) { (name: String) =>
      whenever(!isValidName(name)) {
        assertResult(None) {
          Atom.build(name)
        }
      }
    }
  }

  property("[Atom.build()] names without invalid chars") {
    forAll { (name: String) =>
      whenever(isValidName(name)) {
        assertResult(Some(name)) {
          Atom.build(name).map(_.name)
        }
      }
    }
  }

  property("[Atom.buildWithoutValidation()] empty name") {
    assertResult("") {
      Atom.buildWithoutValidation("").name
    }
  }

  property("[Atom.buildWithoutValidation()] names with invalid chars") {
    forAll(genStringContainingAnyOf(InvalidNameChars)) { (name: String) =>
      whenever(!isValidName(name)) {
        assertResult(name) {
          Atom.buildWithoutValidation(name).name
        }
      }
    }
  }

  property("[Atom.buildWithoutValidation()] names without invalid chars") {
    forAll { (name: String) =>
      whenever(isValidName(name)) {
        assertResult(name) {
          Atom.buildWithoutValidation(name).name
        }
      }
    }
  }

  property("[Group.build()] less than two children") {
    forAll { (formulaOpt: Option[Formula[String]]) =>
      // Don't need `whenever` here; shrinks will preserve the condition
      assertResult(None) {
        Group.build(formulaOpt)
      }
    }
  }

  property("[Group.build()] two or more children") {
    forAll { (formulas: List[Formula[String]]) =>
      whenever(formulas.size >= 2) {
        assertResult(Some(formulas)) {
          Group.build(formulas).map(_.children)
        }
      }
    }
  }

  property("[Group.buildWithoutValidation()] less than two children") {
    forAll { (formulaOpt: Option[Formula[String]]) =>
      // Don't need `whenever` here; shrinks will preserve the condition
      assertResult(formulaOpt.toSeq) {
        Group.buildWithoutValidation(formulaOpt).children
      }
    }
  }

  property("[Group.buildWithoutValidation()] two or more children") {
    forAll { (formulas: List[Formula[String]]) =>
      whenever(formulas.size >= 2) {
        assertResult(formulas) {
          Group.buildWithoutValidation(formulas).children
        }
      }
    }
  }

  property("[Formula.render()] definition") {
    forAll { (formula: Formula[String]) =>
      // Don't need `whenever` here; gens and shrinks for `Formula` are valid
      assertResult(declarativeRender(formula)) {
        formula.render
      }
    }
  }

  property("[Formula.parse()] inverse of rendering") {
    forAll { (formula: Formula[String]) =>
      // Don't need `whenever` here; gens and shrinks for `Formula` are valid
      assertResult(Right(formula)) {
        Formula.parse(formula.render)
      }
    }
  }

}

object FormulaSpec {

  def isValidName(name: String): Boolean = {
    name.nonEmpty && !name.exists(InvalidNameChars)
  }

  def declarativeRender(formula: Formula[String]): String = {
    formula match {
      case Atom(name) => name
      case Group(children) =>
        "(" + children.map(declarativeRender).mkString(" ") + ")"
    }
  }

}

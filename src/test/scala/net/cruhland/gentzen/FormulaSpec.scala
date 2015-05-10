package net.cruhland.gentzen

import org.scalatest._
import prop._
import org.scalacheck._
import Arbitrary.arbitrary
import Shrink.shrink

import FormulaGen._

class FormulaSpec extends GentzenSpec {

  import FormulaSpec._

  property("[Constant.build()] empty value") {
    assertResult(None) {
      Constant.build("")
    }
  }

  property("[Constant.build()] values with invalid chars") {
    forAll(genStringContainingAnyOf(InvalidAtomChars)) { (value: String) =>
      whenever(!isValidConstant(value)) {
        assertResult(None) {
          Constant.build(value)
        }
      }
    }
  }

  property("[Constant.build()] values without invalid chars") {
    forAll { (value: String) =>
      whenever(isValidConstant(value)) {
        assertResult(Some(value)) {
          Constant.build(value).map(_.value)
        }
      }
    }
  }

  property("[Constant.buildWithoutValidation()] empty value") {
    assertResult("") {
      Constant.buildWithoutValidation("").value
    }
  }

  property("[Constant.buildWithoutValidation()] values with invalid chars") {
    forAll(genStringContainingAnyOf(InvalidAtomChars)) { (value: String) =>
      whenever(!isValidConstant(value)) {
        assertResult(value) {
          Constant.buildWithoutValidation(value).value
        }
      }
    }
  }

  property("[Constant.buildWithoutValidation()] values without invalid chars") {
    forAll { (value: String) =>
      whenever(isValidConstant(value)) {
        assertResult(value) {
          Constant.buildWithoutValidation(value).value
        }
      }
    }
  }

  property("[Group.build()] less than two children") {
    forAll { (formulaOpt: Option[Formula]) =>
      // Don't need `whenever` here; shrinks will preserve the condition
      assertResult(None) {
        Group.build(formulaOpt)
      }
    }
  }

  property("[Group.build()] two or more children") {
    forAll { (formulas: List[Formula]) =>
      whenever(formulas.size >= 2) {
        assertResult(Some(formulas)) {
          Group.build(formulas).map(_.children)
        }
      }
    }
  }

  property("[Group.buildWithoutValidation()] less than two children") {
    forAll { (formulaOpt: Option[Formula]) =>
      // Don't need `whenever` here; shrinks will preserve the condition
      assertResult(formulaOpt.toSeq) {
        Group.buildWithoutValidation(formulaOpt).children
      }
    }
  }

  property("[Group.buildWithoutValidation()] two or more children") {
    forAll { (formulas: List[Formula]) =>
      whenever(formulas.size >= 2) {
        assertResult(formulas) {
          Group.buildWithoutValidation(formulas).children
        }
      }
    }
  }

  property("[Formula.render()] definition") {
    forAll { (formula: Formula) =>
      // Don't need `whenever` here; gens and shrinks for `Formula` are valid
      assertResult(declarativeRender(formula)) {
        formula.render
      }
    }
  }

  property("[Formula.parse()] inverse of rendering") {
    forAll { (formula: Formula) =>
      // Don't need `whenever` here; gens and shrinks for `Formula` are valid
      assertResult(Right(formula)) {
        Formula.parse(formula.render)
      }
    }
  }

}

object FormulaSpec {

  def isValidConstant(value: String): Boolean = {
    value.nonEmpty && !value.exists(InvalidAtomChars)
  }

  def declarativeRender(formula: Formula): String = {
    formula match {
      case Atom(Constant(value)) => value
      case Atom(_) =>
        val message = "rendering non-constants is undefined"
        throw new IllegalArgumentException(message)
      case Group(children) =>
        "(" + children.map(declarativeRender).mkString(" ") + ")"
    }
  }

}

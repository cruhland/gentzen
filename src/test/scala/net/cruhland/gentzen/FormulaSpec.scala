package net.cruhland.gentzen

import org.scalatest._
import prop._
import org.scalacheck._
import Arbitrary.arbitrary
import Shrink.shrink

class FormulaSpec extends PropSpec with PropertyChecks {

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

  val InvalidNameChars: Set[Char] = Set(' ', '(', ')')

  def isValidName(name: String): Boolean = {
    name.nonEmpty && !name.exists(InvalidNameChars)
  }

  def genAtom: Gen[Atom] = {
    val genValidChar = arbitrary[Char].suchThat(!InvalidNameChars(_))
    for {
      chars <- actuallyNonEmptyListOf(genValidChar)
    } yield {
      // Call `buildWithoutValidation()` since `chars` is already valid
      Atom.buildWithoutValidation(new String(chars.toArray))
    }
  }

  def genGroup: Gen[Group] = {
    Gen.sized { size =>
      for {
        n <- Gen.choose(0, 3)
        numChildren = n + 2
        childSize = size / numChildren
        sizedFormula = Gen.resize(childSize, genFormula)
        firstFormula <- sizedFormula
        secondFormula <- sizedFormula
        otherFormulas <- Gen.listOfN(n, sizedFormula)
      } yield {
        // Call `buildWithoutValidation()` because children has >= 2 elements
        val children = firstFormula :: secondFormula :: otherFormulas
        Group.buildWithoutValidation(children)
      }
    }
  }

  def genFormula: Gen[Formula] = {
    Gen.sized { size =>
      if (size < 2) genAtom
      else Gen.oneOf(genAtom, Gen.resize(size, genGroup))
    }
  }

  implicit val arbFormula: Arbitrary[Formula] = {
    Arbitrary(genFormula)
  }

  implicit val shrinkFormula: Shrink[Formula] = {
    Shrink { formula =>
      formula match {
        case Atom(name) => shrink(name).flatMap(Atom.build)
        case Group(children) => shrink(children).flatMap(Group.build)
      }
    }
  }

  def declarativeRender(formula: Formula): String = {
    formula match {
      case Atom(name) => name
      case Group(children) =>
        "(" + children.map(declarativeRender).mkString(" ") + ")"
    }
  }

}

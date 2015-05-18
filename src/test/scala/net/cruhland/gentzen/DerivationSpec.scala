package net.cruhland.gentzen

import FormulaGen._

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

object DerivationSpec {

  def genTemplate: Gen[Formula] = {
    for {
      // TODO: Rename `genFormula` to indicate that it's a simple formula only
      formula <- FormulaGen.genFormula
      atomGroups <- GenExtras.partition(countAtoms(formula))
      templateAtoms <- GenExtras.tempGenAtoms(atomGroups)
    } yield replaceAtoms(formula, templateAtoms)
  }

  def countAtoms(formula: Formula): Int = {
    formula match {
      case Atom(_) => 1
      case Group(children) => children.map(countAtoms).sum
    }
  }

  // TODO: Clean up and combine with other functions to make template gen
  def replaceAtoms(f: Formula, atoms: List[AtomValue]): Formula = {
    def helper(
      f: Formula, atoms: List[AtomValue]
    ): (Formula, List[AtomValue]) = {
      atoms match {
        case a :: as =>
          f match {
            case Atom(_) => (Atom(a), as)
            case Group(children) =>
              val initialState = (List.empty[Formula], atoms)
              val (revChildren, leftoverAtoms) =
                children.foldLeft(initialState) { (state, child) =>
                  val (updatedChildren, remainingAtoms) = state
                  val (updatedChild, updatedAtoms) =
                    helper(child, remainingAtoms)
                  (updatedChild :: updatedChildren, updatedAtoms)
                }
              (Group.buildWithoutValidation(revChildren.reverse), leftoverAtoms)
          }
        case _ => (f, atoms)
      }
    }

    val (newFormula, _) = helper(f, atoms)
    newFormula
  }

}

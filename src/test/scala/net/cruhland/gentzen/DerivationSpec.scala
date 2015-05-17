package net.cruhland.gentzen

import FormulaGen._

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

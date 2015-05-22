package net.cruhland.gentzen

import org.scalacheck._
import org.scalatest._

class DerivationSpec extends FreeSpec {

  import DerivationSpec._

  "a template" - {

    "of a single constant atom" - {

      "only matches a formula with an identical constant" in {
        assertResult(true)(Derivation.matches(constant("0"), constant("0")))
        assertResult(false)(Derivation.matches(constant("1"), constant("2")))
        assertResult(false) {
          Derivation.matches(constant("2"), group(constant("3"), constant("4")))
        }
      }

    }

    "of a single variable atom" - {

      "matches any simple formula" in {
        assertResult(true)(Derivation.matches(variable("A"), constant("0")))
        assertResult(true) {
          Derivation.matches(variable("B"), group(constant("1"), constant("2")))
        }
      }

    }

    "of a single variable atom with occurrence check" - {

      "matches iff provided predicate matches" in {
        val template = variable("A", "x")
        val x = constant("x")
        val y = constant("y")
        assertResult(true)(Derivation.matches(template, x, varOccurs))
        assertResult(false)(Derivation.matches(template, y, varOccurs))
      }

    }

    "of a group" - {

      "does not match a non-group" in {
        val template = group(constant("0"), variable("A"))
        assertResult(false)(Derivation.matches(template, constant("1")))
      }

      "does not match a group with a different number of children" in {
        val template = group(constant("1"), variable("B"), constant("2"))
        val smallerGroup = group(constant("1"), constant("2"))
        val largerGroup =
          group(constant("1"), constant("2"), constant("3"), constant("4"))
        assertResult(false)(Derivation.matches(template, smallerGroup))
        assertResult(false)(Derivation.matches(template, largerGroup))
      }

      "does not match a group with a non-matching child" in {
        val constTemplate = group(constant("1"), constant("0"))
        val leftDiff = group(constant("2"), constant("0"))
        val rightDiff = group(constant("1"), constant("3"))
        val bothDiff = group(constant("4"), constant("5"))
        assertResult(false)(Derivation.matches(constTemplate, leftDiff))
        assertResult(false)(Derivation.matches(constTemplate, rightDiff))
        assertResult(false)(Derivation.matches(constTemplate, bothDiff))

        val groupTemplate = group(constTemplate, constant("2"))
        val nonGroupDiff = group(constant("3"), constant("2"))
        val bigGroup = group(constant("1"), constant("0"), constant("3"))
        val numChildDiff = group(bigGroup, constant("2"))
        val mismatchGroup = group(constant("3"), constant("0"))
        val recursiveDiff = group(mismatchGroup, constant("2"))
        assertResult(false)(Derivation.matches(groupTemplate, nonGroupDiff))
        assertResult(false)(Derivation.matches(groupTemplate, numChildDiff))
        assertResult(false)(Derivation.matches(groupTemplate, recursiveDiff))
      }

      "matches a group with corresponding matching children" in {
        val constTemplate = group(constant("1"), constant("0"))
        assertResult(true)(Derivation.matches(constTemplate, constTemplate))

        val varTemplate = group(variable("A"), variable("B"))
        assertResult(true)(Derivation.matches(varTemplate, constTemplate))

        val nestedTemplate = group(constTemplate, varTemplate)
        val nestedFormula = group(constTemplate, constTemplate)
        assertResult(true)(Derivation.matches(nestedTemplate, nestedFormula))
      }

    }

    "with more than one occurrence of the same variable" - {

      "only matches if all occurrences identify the same subformula" in {
        val template = group(variable("A"), variable("A"))
        val mismatchedGroup = group(constant("0"), constant("1"))
        val matchedGroup = group(constant("0"), constant("0"))
        assertResult(false)(Derivation.matches(template, mismatchedGroup))
        assertResult(true)(Derivation.matches(template, matchedGroup))
      }

    }

  }

}

object DerivationSpec {

  def constant(value: String): Formula = {
    Atom(Constant.build(value).get)
  }

  def variable(name: String): Formula = {
    Atom(FormulaVariable(name, None))
  }

  def variable(name: String, checkVar: String): Formula = {
    Atom(FormulaVariable(name, Some(checkVar)))
  }

  def group(children: Formula*): Formula = {
    Group.build(children).get
  }

  def varOccurs(formula: Formula, variable: String): Boolean = {
    formula match {
      case Atom(Constant(v)) => v == variable
      case Group(children) =>
        children.map(varOccurs(_, variable)).reduce(_ || _)
      case _ => false
    }
  }

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

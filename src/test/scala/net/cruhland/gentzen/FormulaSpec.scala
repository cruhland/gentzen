package net.cruhland.gentzen

import org.scalatest._
import prop._
import org.scalacheck._
import Arbitrary.arbitrary
import Shrink.shrink

class FormulaSpec extends PropSpec with PropertyChecks {

  import FormulaSpec._

  property("rendering a formula") {
    forAll { (formulaModel: FormulaModel) =>
      whenever (isValidFormulaModel(formulaModel)) {
        assertResult(renderModel(formulaModel)) {
          convertModel(formulaModel).render
        }
      }
    }
  }

  property("parsing is the inverse of rendering") {
    forAll { (formulaModel: FormulaModel) =>
      whenever (isValidFormulaModel(formulaModel)) {
        val formula = convertModel(formulaModel)
        assertResult(Right(formula)) {
          Formula.parse(formula.render)
        }
      }
    }
  }

}

object FormulaSpec {

  sealed trait FormulaModel
  case class AtomModel(name: String) extends FormulaModel
  case class GroupModel(
    fm1: FormulaModel, fm2: FormulaModel, fms: List[FormulaModel]
  ) extends FormulaModel

  def isValidFormulaModel(formulaModel: FormulaModel): Boolean = {
    formulaModel match {
      case AtomModel("") => false
      case AtomModel(name) =>
        name.forall(c => !(c == ' ' || c == '(' || c == ')'))
      case GroupModel(fm1, fm2, fms) =>
        isValidFormulaModel(fm1) && isValidFormulaModel(fm2) &&
          fms.map(isValidFormulaModel).reduceOption(_ && _).getOrElse(true)
    }
  }

  def genAtomModel: Gen[AtomModel] = {
    arbitrary[String].flatMap(AtomModel.apply)
  }

  def genGroupModel(size: Int): Gen[GroupModel] = {
    for {
      n <- Gen.choose(0, 3)
      numChildren = n + 2
      childSize = size / numChildren
      sizedFormulaModel = genFormulaModel(childSize)
      fm1 <- sizedFormulaModel
      fm2 <- sizedFormulaModel
      fms <- Gen.listOfN(n, sizedFormulaModel)
    } yield GroupModel(fm1, fm2, fms)
  }

  def genFormulaModel(size: Int): Gen[FormulaModel] = {
    if (size < 2) genAtomModel
    else Gen.oneOf(genAtomModel, genGroupModel(size))
  }

  implicit val arbFormulaModel: Arbitrary[FormulaModel] = {
    Arbitrary(Gen.sized(genFormulaModel))
  }

  implicit val shrinkFormulaModel: Shrink[FormulaModel] = Shrink {
    fm => fm match {
      case AtomModel(name) => shrink(name).map(AtomModel.apply)
      case GroupModel(fm1, fm2, fms) =>
        shrink(fm1).map(GroupModel(_, fm2, fms)) ++
          shrink(fm2).map(GroupModel(fm1, _, fms)) ++
          shrink(fms).map(GroupModel(fm1, fm2, _))
    }
  }

  def convertModel(fm: FormulaModel): Formula = {
    fm match {
      case AtomModel(name) => Atom(name)
      case GroupModel(fm1, fm2, fms) =>
        Group(convertModel(fm1) :: convertModel(fm2) :: fms.map(convertModel))
    }
  }

  def renderModel(fm: FormulaModel): String = {
    fm match {
      case AtomModel(name) => name
      case GroupModel(fm1, fm2, fms) =>
        "(" + renderModel(fm1) + " " + renderModel(fm2) +
        renderRemaining(fms) + ")"
    }
  }

  def renderRemaining(fms: List[FormulaModel]): String = {
    fms match {
      case Nil => ""
      case head :: tail => " " + renderModel(head) + renderRemaining(tail)
    }
  }

}

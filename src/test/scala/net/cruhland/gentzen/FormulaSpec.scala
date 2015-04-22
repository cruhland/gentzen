package net.cruhland.gentzen

import org.scalatest._
import prop._
import org.scalacheck._
import Arbitrary.arbitrary

class FormulaSpec extends PropSpec with PropertyChecks {

  import FormulaSpec._

  property("rendering a formula") {
    forAll { (formulaModel: FormulaModel) =>
      assertResult(renderModel(formulaModel)) {
        convertModel(formulaModel).render
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

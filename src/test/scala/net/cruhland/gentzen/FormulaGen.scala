package net.cruhland.gentzen

import org.scalacheck._
import Arbitrary.arbitrary
import Shrink.shrink

object FormulaGen {

  val InvalidAtomChars: Set[Char] = Set(' ', '(', ')')

  def genPlainAtom: Gen[Atom] = {
    val genValidChar = arbitrary[Char].suchThat(!InvalidAtomChars(_))
    for {
      chars <- GenExtras.actuallyNonEmptyListOf(genValidChar)
    } yield {
      // Call `buildWithoutValidation()` since `chars` is already valid
      val constant = Constant.buildWithoutValidation(new String(chars.toArray))
      Atom(constant)
    }
  }

  def genGroup: Gen[Group] = {
    Gen.sized { size =>
      for {
        childSizes <- GenExtras.composition(size - 1).retryUntil(_.size >= 2)
        childrenGens = childSizes.map(Gen.resize(_, genFormula))
        children <- Gen.sequence[Seq[Formula], Formula](childrenGens)
      } yield {
        // Call `buildWithoutValidation()` because children has >= 2 elements
        Group.buildWithoutValidation(children)
      }
    }
  }

  def genFormula: Gen[Formula] = {
    Gen.sized(size => if (size < 3) genPlainAtom else genGroup)
  }

  implicit val arbFormula: Arbitrary[Formula] = {
    Arbitrary(genFormula)
  }

  implicit val shrinkFormula: Shrink[Formula] = {
    Shrink { formula =>
      formula match {
        case Atom(value) => shrink(value).map(Atom.apply)
        case Group(children) => shrink(children).flatMap(Group.build)
      }
    }
  }

}

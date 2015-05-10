package net.cruhland.gentzen

import org.scalacheck._
import Arbitrary.arbitrary
import Shrink.shrink

object FormulaGen {

  val InvalidAtomChars: Set[Char] = Set(' ', '(', ')')

  def genPlainAtom: Gen[Atom] = {
    val genValidChar = arbitrary[Char].suchThat(!InvalidAtomChars(_))
    for {
      chars <- actuallyNonEmptyListOf(genValidChar)
    } yield {
      // Call `buildWithoutValidation()` since `chars` is already valid
      val constant = Constant.buildWithoutValidation(new String(chars.toArray))
      Atom(constant)
    }
  }

  def genGroup: Gen[Group] = {
    Gen.sized { size =>
      for {
        n <- Gen.choose(0, math.min(3, size))
        numChildren = n + 2
        childSize = size / numChildren
        sizedFormula = Gen.resize(childSize, genFormula)
        children <- Gen.listOfN(numChildren, sizedFormula)
      } yield {
        // Call `buildWithoutValidation()` because children has >= 2 elements
        Group.buildWithoutValidation(children)
      }
    }
  }

  def genFormula: Gen[Formula] = {
    Gen.sized { size =>
      if (size < 2) genPlainAtom
      else Gen.oneOf(genPlainAtom, Gen.resize(size, genGroup))
    }
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

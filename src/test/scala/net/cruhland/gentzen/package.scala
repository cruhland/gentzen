package net.cruhland

import org.scalatest._
import prop._
import org.scalacheck._
import Arbitrary.arbitrary
import Shrink.shrink

package object gentzen {

  def genStringContainingAnyOf(chars: Set[Char]): Gen[String] = {
    Gen.sized { size =>
      for {
        numChars <- Gen.choose(1, size)
        charSeq <- Gen.listOfN(numChars, Gen.oneOf(chars.toSeq))
        numStrings = numChars + 1
        stringSize = size / numStrings
        sizedStringGen = Gen.resize(stringSize, arbitrary[String])
        stringSeq <- Gen.listOfN(numStrings, sizedStringGen)
      } yield interleave(stringSeq, charSeq).mkString
    }
  }

  def actuallyNonEmptyListOf[A](genElem: Gen[A]): Gen[List[A]] = {
    for {
      head <- genElem
      tail <- Gen.listOf(genElem)
    } yield head :: tail
  }

  def interleave[A, B >: A](first: List[A], second: List[B]): List[B] = {
    first match {
      case a :: as => a :: interleave(second, as)
      case _ => second
    }
  }

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

  abstract class GentzenSpec extends PropSpec with PropertyChecks

}

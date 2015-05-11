package net.cruhland.gentzen

import org.scalacheck._
import Arbitrary.arbitrary

object GenExtras {

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

  def composition(n: Int): Gen[List[Int]] = {
    for {
      bits <- Gen.listOfN(n - 1, Gen.choose(0, 1))
    } yield {
      bits.foldLeft(List(1)) { (ans, b) =>
        if (b == 0) 1 :: ans else 1 + ans.head :: ans.tail
      }
    }
  }

}

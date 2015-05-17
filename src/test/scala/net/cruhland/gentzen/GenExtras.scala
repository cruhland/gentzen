package net.cruhland.gentzen

import scala.util.Random

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

  def partition(n: Int): Gen[List[Int]] = {
    partition(n, n)
  }

  // TODO: Can this be made tail recursive?
  def partition(n: Int, k: Int): Gen[List[Int]] = {
    val possibleElements = 1 to k
    val partitionsWithMaxElement =
      possibleElements
        .view
        .map(m => pmax(n, m) -> m)
        .filter { case (count, _) => count > 0 }
        .map { case (count, element) => count -> Gen.const(element) }

    if (partitionsWithMaxElement.isEmpty) Gen.const(Nil)
    else {
      for {
        element <- Gen.frequency(partitionsWithMaxElement: _*)
        subPartition <- partition(n - element, element)
      } yield element :: subPartition
    }
  }

  // TODO: Memoize
  def pmax(n: Int, k: Int): Int = {
    (n, k) match {
      case (0, 0) => 1
      case _ if n <= 0 || k <= 0 => 0
      case _ => pmax(n - k, k) + pmax(n - 1, k - 1)
    }
  }

  // TODO: Move this into DerivationSpec once it's complete
  def tempGenAtoms(p: List[Int]): Gen[List[AtomValue]] = {
    val genAtomValueConstructor =
      Gen.oneOf(FormulaVariable.apply _, Constant.buildWithoutValidation _)
    val listOfGens = p.zipWithIndex.map {
      case (n, i) => genAtomValueConstructor.map(f => n -> f(i.toString))
    }

    for {
      list <- Gen.sequence[List[(Int, AtomValue)], (Int, AtomValue)](listOfGens)
    } yield {
      Random.shuffle(list.flatMap { case (n, av) => List.fill(n)(av) })
    }
  }

}

package net.cruhland.gentzen

import org.scalatest._
import prop._

import org.scalacheck._
import Arbitrary.arbitrary
import Shrink.shrink

import FormulaGen._

class ProofFigureSpec extends GentzenSpec {

  import ProofFigureSpec._

  property("De/Construction of ProofFigures") {
    // This is trivial, but ensures that proof figures have the required
    // components
    forAll {
      (upperFigures: List[ProofFigure], lowerFormula: Formula) =>
        // We don't need `whenever` because gen and shrinks are always correct
        val proofFigure = ProofFigure(upperFigures, lowerFormula)
        assertResult(upperFigures) {
          proofFigure.upperFigures
        }
        assertResult(lowerFormula) {
          proofFigure.lowerFormula
        }
    }
  }

}

object ProofFigureSpec {

  def genProofFigure: Gen[ProofFigure] = {
    Gen.sized { size =>
      for {
        numUpper <- Gen.choose(0, math.min(5, size))
        upperSize = if (numUpper > 0) size / numUpper else 0
        sizedProofFigure = Gen.resize(upperSize, genProofFigure)
        upperFigures <- Gen.listOfN(numUpper, sizedProofFigure)
        lowerFormula <- arbitrary[Formula]
      } yield ProofFigure(upperFigures, lowerFormula)
    }
  }

  implicit val arbProofFigure: Arbitrary[ProofFigure] = {
    Arbitrary(genProofFigure)
  }

  implicit val shrinkProofFigure: Shrink[ProofFigure] = {
    Shrink { pf =>
      val uppers = shrink(pf.upperFigures).map(uf => pf.copy(upperFigures = uf))
      val lowers = shrink(pf.lowerFormula).map(lf => pf.copy(lowerFormula = lf))
      uppers ++ lowers
    }
  }

}

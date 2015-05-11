package net.cruhland.gentzen

import org.scalacheck._

object GenExtras {

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

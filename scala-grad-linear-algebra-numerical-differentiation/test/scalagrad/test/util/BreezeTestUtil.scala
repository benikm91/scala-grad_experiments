package scalagrad.test.util

import scalagrad.test.util.TestUtil.{reasonableDoubleGenerator, isReasonableDouble}
import org.scalacheck.Gen
import breeze.linalg.DenseVector

object BreezeTestUtil:

    def reasonableDenseVectorDoubleGenerator: Gen[DenseVector[Double]] = 
      for
        n <- Gen.choose(1, 100)
        xs <- Gen.listOfN(n, reasonableDoubleGenerator)
      yield DenseVector(xs.toArray)
    
    def isReasonableDenseVectorDouble(x: DenseVector[Double]): Boolean = 
        x.forall(isReasonableDouble)
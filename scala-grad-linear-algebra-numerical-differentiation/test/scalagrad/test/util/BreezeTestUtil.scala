package scalagrad.test.util

import scalagrad.test.util.TestUtil.{reasonableDoubleGenerator, isReasonableDouble}
import org.scalacheck.Gen
import breeze.linalg.DenseVector
import breeze.linalg.DenseMatrix

object BreezeTestUtil:

    def reasonableDenseMatrixDoubleGenerator: Gen[DenseMatrix[Double]] = reasonableDenseMatrixDoubleGenerator(reasonableDoubleGenerator)
    def reasonableDenseMatrixDoubleGenerator(doubleGenerator: Gen[Double]): Gen[DenseMatrix[Double]] =
      for
        nRows <- Gen.choose(5, 10)
        nCols <- Gen.choose(5, 10)
        xs <- Gen.listOfN(nRows * nCols, doubleGenerator)
      yield new DenseMatrix(nRows, nCols, xs.toArray)

    def isReasonableDenseMatrixDouble(x: DenseMatrix[Double]): Boolean = 
        x.forall(isReasonableDouble)

    def reasonableDenseVectorDoubleGenerator: Gen[DenseVector[Double]] = reasonableDenseVectorDoubleGenerator(reasonableDoubleGenerator)
    def reasonableDenseVectorDoubleGenerator(doubleGenerator: Gen[Double]): Gen[DenseVector[Double]] = 
      for
        n <- Gen.choose(1, 100)
        xs <- Gen.listOfN(n, doubleGenerator)
      yield DenseVector(xs.toArray)
    
    def isReasonableDenseVectorDouble(x: DenseVector[Double]): Boolean = 
        x.forall(isReasonableDouble)
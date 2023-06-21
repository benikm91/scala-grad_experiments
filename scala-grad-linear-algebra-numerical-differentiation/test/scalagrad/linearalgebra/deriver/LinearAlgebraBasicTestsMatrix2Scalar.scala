package scalagrad.linearalgebra.deriver

import scalagrad.api.Deriver
import scalagrad.api.ScalaGrad
import scalagrad.api.linearalgebra.LinearAlgebraOps

import scala.reflect.ClassTag

import collection.mutable.Stack
import org.scalatest.*
import flatspec.*
import matchers.*
import org.scalatest.prop.TableDrivenPropertyChecks.whenever
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks.forAll
import org.scalacheck.Gen
import org.scalacheck.Prop.forAllNoShrink

import scalagrad.test.util.BreezeTestUtil.*

import scalagrad.linearalgebra.numerical.DeriverLinearAlgebraNumerical.*
import breeze.linalg.{DenseVector, zipValues}
import scalagrad.api.linearalgebra.ScalaLinearAlgebraOpsFor
import scalagrad.linearalgebra.api.BreezeVectorAlgebraForDouble
import breeze.linalg.DenseMatrix
import spire.algebra.Trig
import spire.implicits.DoubleAlgebra
import scalagrad.test.util.TestUtil.reasonableDoubleGenerator

trait LinearAlgebraBasicTestsMatrix2Scalar(
    ops: LinearAlgebraOps,
    deriverM: Deriver[ops.Matrix => ops.Scalar] {
        type dfT = DenseMatrix[Double] => DenseMatrix[Double]
    },
)(
  using trig: Trig[ops.Scalar]
) extends AnyWordSpec with should.Matchers:

  def name: String

  def testM(f: (ops: LinearAlgebraOps) => (v: ops.Matrix) => ops.Scalar, tolerance: Double = 0.1, doubleGenerator: Gen[Double] = reasonableDoubleGenerator) = 
    forAll(reasonableDenseMatrixDoubleGenerator(doubleGenerator)) { (x: DenseMatrix[Double]) =>
      whenever(isReasonableDenseMatrixDouble(x)) {
        val df = ScalaGrad.derive(f(ops))(using deriverM)
        val dx = df(x)
        dx.rows should be(x.rows)
        dx.cols should be(x.cols)
        val approxDx = ScalaGrad.derive(f(BreezeVectorAlgebraForDouble))(using approxMatrix(1e-6))(x)
        approxDx.rows should be(x.rows)
        approxDx.cols should be(x.cols)
        dx.toArray.zip(approxDx.toArray).foreach { (dxE, approxDxE) =>
            dxE should be(approxDxE +- tolerance)
        }
      }
    }

  f"${name} deriviation for matrix" should {
    "work for element at" in {
      testM((ops: LinearAlgebraOps) => (x: ops.Matrix) =>
        x.elementAt(0, 1) * 5
      )
    }
    "work for element at twice" in {
      testM((ops: LinearAlgebraOps) => (x: ops.Matrix) =>
        x.elementAt(0, 3) * x.elementAt(2, 1)
      )
    }
    "work for map rows" in {
      testM((ops: LinearAlgebraOps) => (x: ops.Matrix) =>
        x.map(_ * 5)
        x.mapRows(_ * 10).sum
      )
    }
    "work for row at" in {
      testM((ops: LinearAlgebraOps) => (x: ops.Matrix) =>
        x.rowAt(0) * x.rowAt(1).T
      )
    }
    "work for scalar addition" in {
      testM((ops: LinearAlgebraOps) => (x: ops.Matrix) =>
        (x + 10).sum
      )
    }
    "work for row addition" in {
      testM((ops: LinearAlgebraOps) => (x: ops.Matrix) =>
        (x + x.rowAt(0)).sum
      )
    }
    "work for minus" in {
      testM((ops: LinearAlgebraOps) => (x: ops.Matrix) =>
        (x - x).sum
      )
    }
    "work for sum" in {
      testM((ops: LinearAlgebraOps) => (x: ops.Matrix) =>
        x.sum
      )
    }
    "work for sum with recreate row-wise matrix" in {
      testM((ops: LinearAlgebraOps) => (x: ops.Matrix) =>
        val m = ops.stackRowsSeq(
          for 
            i <- 0 until x.rows
          yield x.rowAt(i)
        )
        m.sum
      )
    }
    "work for map" in {
      testM((ops: LinearAlgebraOps) => (x: ops.Matrix) =>
        x.map(_ * 10).sum
      )
    }
    "work for map2" in {
      testM((ops: LinearAlgebraOps) => (x: ops.Matrix) =>
        val c = x.sum
        x.map2(_ + c).sum
      )
    }
    "work for everything all at once"  in {
      testM((ops: LinearAlgebraOps) => (x: ops.Matrix) =>
        val m = ops.stackRowsSeq(
          for 
            i <- 0 until x.rows
          yield x.rowAt(i)
        )
        val c = x.rowAt(0) * 10
        val cv2 = (c * x.T) + 5
        (cv2 / 3.5).T.sum
      )
    }
  }

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

import scalagrad.test.util.BreezeTestUtil.{reasonableDenseVectorDoubleGenerator, isReasonableDenseVectorDouble}

import scalagrad.linearalgebra.numerical.DeriverLinearAlgebraNumerical.approxVector
import breeze.linalg.{DenseVector, zipValues}
import scalagrad.api.linearalgebra.ScalaLinearAlgebraOpsFor
import scalagrad.linearalgebra.api.BreezeVectorAlgebraForDouble
import breeze.linalg.DenseMatrix

abstract class LinearAlgebraBasicTests(
    val name: String, 
    ops: LinearAlgebraOps,
    deriver: Deriver[ops.ColumnVector => ops.Scalar] {
        type dfT = DenseVector[Double] => DenseVector[Double]
    }
) extends AnyWordSpec with should.Matchers:

  def testCV(f: (ops: LinearAlgebraOps) => (v: ops.ColumnVector) => ops.Scalar) = 
    val tolerance = 1e-2
    forAll(reasonableDenseVectorDoubleGenerator) { (x: DenseVector[Double]) =>
      whenever(isReasonableDenseVectorDouble(x)) {
        val df = ScalaGrad.derive(f(ops))(using deriver)
        val dx = df(x)
        val approxDx = ScalaGrad.derive(f(BreezeVectorAlgebraForDouble))(using approxVector(1e-6))(x)
        zipValues(dx, approxDx).foreach { (dxE, approxDxE) =>
            dxE should be(approxDxE +- tolerance)
        }
      }
    }

  f"${name} deriviation" should {
    "work for sum reduction" in {
      testCV((ops: LinearAlgebraOps) => (x: ops.ColumnVector) =>
        x.sum
      )
    }
    "work for constant addition" in {
      testCV((ops: LinearAlgebraOps) => (x: ops.ColumnVector) =>
        (x + 10.0).sum
      )
    }
    "work for constant multiplication" in {
      testCV((ops: LinearAlgebraOps) => (x: ops.ColumnVector) =>
        (x * 10.0).sum
      )
    }
    "work for constant division" in {
      testCV((ops: LinearAlgebraOps) => (x: ops.ColumnVector) =>
        (x / 5.0).sum
      )
    }
    "work for inner vector dot product" in {
      testCV((ops: LinearAlgebraOps) => (x: ops.ColumnVector) =>
        x.T * x
      )
    }
    "work for element wise" in {
      testCV((ops: LinearAlgebraOps) => (x: ops.ColumnVector) =>
        x.map(_ * 10.0).sum
      )
    }
    "work with a mixture of all operations" in {
      testCV((ops: LinearAlgebraOps) => (x: ops.ColumnVector) =>
        (x * 10.0 + x * 20.0 - x.map(_ / 2) / 5.0 + x / 3.0).map(_ * 10.0).sum
      )
    }
  }

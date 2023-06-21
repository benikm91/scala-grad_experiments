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

abstract class LinearAlgebraBasicTests(
    val name: String, 
    ops: LinearAlgebraOps,
    deriverM: Deriver[ops.Matrix => ops.Scalar] {
        type dfT = DenseMatrix[Double] => DenseMatrix[Double]
    },
    deriverCV: Deriver[ops.ColumnVector => ops.Scalar] {
        type dfT = DenseVector[Double] => DenseVector[Double]
    },
)(
  using trig: Trig[ops.Scalar]
) extends LinearAlgebraBasicTestsColumnVector2Scalar(
  ops, deriverCV
) with LinearAlgebraBasicTestsMatrix2Scalar(
  ops, deriverM
)

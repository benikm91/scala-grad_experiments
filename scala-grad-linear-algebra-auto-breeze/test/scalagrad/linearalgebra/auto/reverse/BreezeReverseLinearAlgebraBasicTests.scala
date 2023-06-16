package scalagrad.spire.auto.forward.test

import collection.mutable.Stack
import org.scalatest.*
import flatspec.*
import matchers.*
import org.scalatest.prop.TableDrivenPropertyChecks.whenever
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks.forAll
import org.scalacheck.Gen
import org.scalacheck.Prop.forAllNoShrink
import scalagrad.api.Deriver
import scalagrad.api.ScalaGrad

import scalagrad.linearalgebra.deriver.LinearAlgebraBasicTests
import scalagrad.api.linearalgebra.LinearAlgebraOps

import spire.math.Numeric
import spire.implicits.*
import scalagrad.linearalgebra.auto.reverse.DeriverBreezeReversePlan
import scalagrad.auto.reverse.DeriverReversePlan.given
import scalagrad.linearalgebra.auto.reverse.BreezeVectorAlgebraForDualDeltaDouble
import scalagrad.spire.auto.dual.DualIsNumeric.given
import breeze.linalg.DenseVector

class BreezeReverseLinearAlgebraBasicTests extends LinearAlgebraBasicTests(
  "reverse-mode",
  BreezeVectorAlgebraForDualDeltaDouble,
  DeriverBreezeReversePlan.matrix2Scalar,
  DeriverBreezeReversePlan.vector2Scalar
)
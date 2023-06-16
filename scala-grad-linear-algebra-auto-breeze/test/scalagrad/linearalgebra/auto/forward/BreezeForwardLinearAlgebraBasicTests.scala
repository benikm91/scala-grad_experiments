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
import scalagrad.linearalgebra.auto.forward.DeriverBreezeForwardPlan
import scalagrad.auto.forward.DeriverForwardPlan.given
import scalagrad.spire.auto.dual.DualIsNumeric.given
import scalagrad.linearalgebra.auto.forward.BreezeVectorAlgebraForDualNumberDouble

import breeze.linalg.DenseVector

class BreezeForwardLinearAlgebraBasicTests extends LinearAlgebraBasicTests(
  "forward-mode",
  BreezeVectorAlgebraForDualNumberDouble,
  DeriverBreezeForwardPlan.matrix2Scalar,
  DeriverBreezeForwardPlan.vector2Scalar
)
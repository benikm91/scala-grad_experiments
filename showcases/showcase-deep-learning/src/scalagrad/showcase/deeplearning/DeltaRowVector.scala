package scalagrad.showcase.deeplearning

import scalagrad.api.ScalaGrad
import scalagrad.api.Dual
import scalagrad.auto.forward.dual.DualNumber
import scalagrad.fractional.auto.dual.DualIsFractional.given
import scalagrad.auto.reverse.dual.DualDelta
import breeze.linalg.{Vector => _, *}
import breeze.linalg.operators._
import breeze.linalg.support._
import scalagrad.api.Deriver
import scalagrad.api.DeriverPlan
import scala.reflect.ClassTag
import scala.math.Fractional
import scala.runtime.Tuples
import scalagrad.auto.forward.dual.DualNumber

enum DeltaRowVector[P]:
    case Zero(x: P)
    case Val(id: Int)
    case Transpose(v: DeltaColumnVector[P])
    case MatrixDot(v: breeze.linalg.Transpose[DenseVector[P]], d: DeltaMatrix[P])
    case MatrixDot2(d: DeltaRowVector[P], v: DenseMatrix[P])
    case AddVV(d1: DeltaRowVector[P], d2: DeltaRowVector[P])
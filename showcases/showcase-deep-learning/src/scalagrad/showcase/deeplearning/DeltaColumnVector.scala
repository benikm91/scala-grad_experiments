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

enum DeltaColumnVector[P]:
    case Zero(x: P)
    case Val(id: Int)
    case Transpose(v: DeltaRowVector[P])
    case MatrixDot(d: DeltaMatrix[P], v: DenseVector[P])
    case MatrixDot2(v: DenseMatrix[P], d: DeltaColumnVector[P])
    case AddVV(d1: DeltaColumnVector[P], d2: DeltaColumnVector[P])
    case AddVS(d: DeltaColumnVector[P], s: DeltaScalar[P])
    case MinusVV(d1: DeltaColumnVector[P], d2: DeltaColumnVector[P])
    case ElementWiseScale(v: DenseVector[P], d: DeltaColumnVector[P])
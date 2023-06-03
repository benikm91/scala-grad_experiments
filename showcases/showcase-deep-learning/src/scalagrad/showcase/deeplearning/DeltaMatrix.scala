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

enum DeltaMatrix[P]:
    case Zero(x: P)
    case Val(id: Int)
    case Transpose(m: DeltaMatrix[P])
    case MatrixDotMDM(m: DenseMatrix[P], d: DeltaMatrix[P])
    case MatrixDotDMM(d: DeltaMatrix[P], m: DenseMatrix[P])
    case MatrixDotDCVRV(d: DeltaColumnVector[P], v: breeze.linalg.Transpose[DenseVector[P]])
    case MatrixDotCVDRV(v: DenseVector[P], d: DeltaRowVector[P])
    case AddDMDM(m1: DeltaMatrix[P], m2: DeltaMatrix[P])
    case AddDMDCV(m: DeltaMatrix[P], v: DeltaColumnVector[P])
    case ElementWiseScale(v: DenseMatrix[P], d: DeltaMatrix[P])

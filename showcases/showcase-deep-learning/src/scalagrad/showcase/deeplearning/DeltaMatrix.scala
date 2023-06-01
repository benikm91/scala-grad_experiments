package scalagrad.showcase.deeplearning

import scalagrad.api.ScalaGrad
import scalagrad.api.Dual
import scalagrad.api.VectorAlgebraFor
import scalagrad.api.VectorAlgebraOps
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
    case MatrixDot(m: DenseMatrix[P], d: DeltaMatrix[P])
    case MatrixDot2(d: DeltaMatrix[P], m: DenseMatrix[P])
    case MatrixDot3(d: DeltaColumnVector[P], v: breeze.linalg.Transpose[DenseVector[P]])
    case MatrixDot4(v: DenseVector[P], d: DeltaRowVector[P])
    case AddMM(m1: DeltaMatrix[P], m2: DeltaMatrix[P])
    case AddMV(m: DeltaMatrix[P], v: DeltaColumnVector[P])
    case ElementWiseScale(v: DenseMatrix[P], d: DeltaMatrix[P])

object DeltaMatrix:

  extension [P](s: DenseMatrix[P])
    def *(d: DeltaMatrix[P]) = DeltaMatrix.MatrixDot(s, d)

  extension [P](d1: DeltaMatrix[P])
    def +(d2: DeltaMatrix[P]) = DeltaMatrix.AddMM(d1, d2)
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

enum DeltaScalar[P]:
    case Zero(x: P) // TODO how to get ride of x here?
    case Val(id: Int)
    case MultiplyVV(m1: DeltaRowVector[P], m2: DeltaColumnVector[P])
    case MultiplyRVDCV(v: Transpose[DenseVector[Double]], d: DeltaColumnVector[P])
    case ColumnDotProduct(v: Transpose[DenseVector[Double]], d: DeltaColumnVector[P])
    case RowDotProduct(d: DeltaRowVector[P], v: DenseVector[Double])
    case Add(s1: DeltaScalar[P], s2: DeltaScalar[P])
    case Sub(s1: DeltaScalar[P], s2: DeltaScalar[P])
    case Div(s1: DeltaScalar[P], f: P)
    case Scale(s1: DeltaScalar[P], f: P)
    case Sum(v: DeltaColumnVector[P], vectorLength: Int)

object DeltaScalar:

  extension [P](s: DeltaScalar[P])
    def +(s2: DeltaScalar[P]) = DeltaScalar.Add(s, s2)
    def -(s2: DeltaScalar[P]) = DeltaScalar.Sub(s, s2)
    def *(p: P) = DeltaScalar.Scale(s, p)
    def /(p: P) = DeltaScalar.Div(s, p)
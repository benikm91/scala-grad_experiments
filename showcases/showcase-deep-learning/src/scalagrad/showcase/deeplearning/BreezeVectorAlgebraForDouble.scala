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

object BreezeVectorAlgebraForDouble extends VectorAlgebraOps:
    
    override type Scalar = Double
    override type ColumnVector = DenseVector[Double]
    override type RowVector = Transpose[DenseVector[Double]]
    override type Matrix = DenseMatrix[Double]

    def liftToScalar(d: Int): Scalar = d.toDouble

    override def transpose(m: Matrix): Matrix = m.t
    override def transposeColumVector(v: ColumnVector): RowVector = v.t
    override def transposeRowVector(v: RowVector): ColumnVector = v.t

    override def timesMM(m1: Matrix, m2: Matrix): Matrix = m1 * m2
    override def timesVV(v1: RowVector, v2: ColumnVector): Scalar = v1 * v2
    override def timesOuterVV(v1: ColumnVector, v2: RowVector): Matrix = v1 * v2
    override def timesVM(v: RowVector, m: Matrix): RowVector = v * m
    override def timesMV(m: Matrix, v: ColumnVector): ColumnVector = m * v
    override def timesVS(v: ColumnVector, s: Scalar): ColumnVector = v * s
    override def timesMS(m: Matrix, s: Scalar): Matrix = m * s

    override def timesSS(s1: Scalar, s2: Scalar): Scalar = s1 * s2

    override def plusMM(m1: Matrix, m2: Matrix): Matrix = m1 + m2
    override def plusVV(v1: ColumnVector, v2: ColumnVector): ColumnVector = v1 + v2
    override def plusMV(m: Matrix, v: ColumnVector): Matrix = 
        m(breeze.linalg.*, ::) + v

    override def plusVS(v: ColumnVector, s: Scalar): ColumnVector = v + s
    override def plusMS(m: Matrix, s: Scalar): Matrix = m + s

    override def plusSS(s1: Scalar, s2: Scalar): Scalar = s1 + s2

    override def minusMM(m1: Matrix, m2: Matrix): Matrix = m1 - m2
    override def minusVV(v1: ColumnVector, v2: ColumnVector): ColumnVector = v1 - v2
    override def minusMV(m: Matrix, v: ColumnVector): Matrix = ???
    override def minusVM(v: ColumnVector, m: Matrix): Matrix = ???
    override def minusVS(v: ColumnVector, s: Scalar): ColumnVector = v - s
    override def minusMS(m: Matrix, s: Scalar): Matrix = m - s

    override def minusSS(s1: Scalar, s2: Scalar): Scalar = s1 - s2

    override def elementWiseOps(v: ColumnVector, f: [T] => T => Numeric[T] ?=> T) = 
        v.map(f[Double])

    def elementWiseOpsM(v: Matrix, f: [T] => (x: T) => (Numeric[T]) ?=> T): Matrix =
        v.map(f[Double])

    override def sum(v: ColumnVector): Scalar = 
        breeze.linalg.sum(v)

    def divideSS(s1: Scalar, s2: Scalar) = s1 / s2

    def length(v: ColumnVector) = v.length
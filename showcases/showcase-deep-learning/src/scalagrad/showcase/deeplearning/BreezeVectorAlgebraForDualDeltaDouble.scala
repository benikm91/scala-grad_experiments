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

object BreezeVectorAlgebraForDualDeltaDouble extends VectorAlgebraOps:
    
    override type Scalar = DualScalar[Double]
    override type ColumnVector = DualColumnVector[Double]
    override type RowVector = DualRowVector[Double]
    override type Matrix = DualMatrix[Double]

    def liftToScalar(d: Int): Scalar = DualScalar(d.toDouble, DeltaScalar.Zero(0.0))

    override def transpose(m: Matrix): Matrix = 
        DualMatrix(m.value.t, DeltaMatrix.Transpose(m.delta))
    override def transposeColumVector(v: ColumnVector): RowVector =  
        DualRowVector(v.value.t, DeltaRowVector.Transpose(v.delta))
    override def transposeRowVector(v: RowVector): ColumnVector =
        DualColumnVector(v.value.t, DeltaColumnVector.Transpose(v.delta))
    override def timesMM(m1: Matrix, m2: Matrix): Matrix =
        def dTimes(v: DenseMatrix[Double], dv: DeltaMatrix[Double], v2: DenseMatrix[Double], dv2: DeltaMatrix[Double]): DeltaMatrix[Double] =
            DeltaMatrix.MatrixDot2(dv, v2) + DeltaMatrix.MatrixDot(v, dv2)
        DualMatrix(m1.value * m2.value, dTimes(m1.value, m1.delta, m2.value, m2.delta))
    override def timesVV(v1: RowVector, v2: ColumnVector): Scalar = 
        def dTimes(v1: Transpose[DenseVector[Double]], dv1: DeltaRowVector[Double], v2: DenseVector[Double], dv2: DeltaColumnVector[Double]): DeltaScalar[Double] =
            DeltaScalar.RowDotProduct(dv1, v2) + DeltaScalar.ColumnDotProduct(v1, dv2)
        DualScalar(v1.value * v2.value, dTimes(v1.value, v1.delta, v2.value, v2.delta))
    override def timesOuterVV(v1: ColumnVector, v2: RowVector): Matrix = 
        def dTimes(v1: DenseVector[Double], dv1: DeltaColumnVector[Double], v2: Transpose[DenseVector[Double]], dv2: DeltaRowVector[Double]): DeltaMatrix[Double] =
            // dv1 * v2 + v1 * dv2 
            DeltaMatrix.MatrixDot4(v1, dv2) + DeltaMatrix.MatrixDot3(dv1, v2)
        DualMatrix(v1.value * v2.value, dTimes(v1.value, v1.delta, v2.value, v2.delta))
    override def timesVM(v: RowVector, m: Matrix): RowVector = 
        def dTimes(v1: Transpose[DenseVector[Double]], dv1: DeltaRowVector[Double], v2: DenseMatrix[Double], dv2: DeltaMatrix[Double]): DeltaRowVector[Double] =
            // dv1 * v2.t + v1 * dv2.t
            DeltaRowVector.AddVV(
                DeltaRowVector.MatrixDot2(dv1, v2.t),
                DeltaRowVector.MatrixDot(v1, DeltaMatrix.Transpose(dv2))
            )
        DualRowVector(v.value * m.value, dTimes(v.value, v.delta, m.value, m.delta))
    override def timesMV(m: Matrix, v: ColumnVector): ColumnVector =
        def dTimes(v1: DenseMatrix[Double], dv1: DeltaMatrix[Double], v2: DenseVector[Double], dv2: DeltaColumnVector[Double]): DeltaColumnVector[Double] =
            DeltaColumnVector.AddVV(
                DeltaColumnVector.MatrixDot(dv1, v2), 
                DeltaColumnVector.MatrixDot2(v1, dv2)
            )
        DualColumnVector(m._1 * v._1, dTimes(m._1, m._2, v._1, v._2))
    override def timesVS(v: ColumnVector, s: Scalar): ColumnVector = ???
    override def timesMS(m: Matrix, s: Scalar): Matrix = ???
    override def timesSS(s1: Scalar, s2: Scalar): Scalar = ???
    override def plusMM(m1: Matrix, m2: Matrix): Matrix = ???
    override def plusVV(v1: ColumnVector, v2: ColumnVector): ColumnVector = ???
    override def plusMV(m: Matrix, v: ColumnVector): Matrix = 
        DualMatrix(
            m._1(breeze.linalg.*, ::) + v._1, 
            DeltaMatrix.AddMV(m.delta, v.delta)
        )
    override def plusVS(v: ColumnVector, s: Scalar): ColumnVector = 
        DualColumnVector(
            v.value + s.value, 
            DeltaColumnVector.AddVS(v.delta, s.delta)
        )
    override def plusMS(m: Matrix, s: Scalar): Matrix = ???
    override def plusSS(s1: Scalar, s2: Scalar): Scalar = 
        DualScalar(s1.value + s2.value, DeltaScalar.Add(s1.delta, s2.delta))
    override def minusMM(m1: Matrix, m2: Matrix): Matrix = ???
    override def minusVV(v1: ColumnVector, v2: ColumnVector): ColumnVector =
        DualColumnVector(
            v1.value - v2.value, 
            DeltaColumnVector.MinusVV(v1.delta, v2.delta)
        )
    override def minusMV(m: Matrix, v: ColumnVector): Matrix = ???
    override def minusVM(v: ColumnVector, m: Matrix): Matrix = ???
    override def minusVS(v: ColumnVector, s: Scalar): ColumnVector = ???
    override def minusMS(m: Matrix, s: Scalar): Matrix = ???

    override def minusSS(s1: Scalar, s2: Scalar): Scalar = ???
    
    override def elementWiseOps(v: ColumnVector, f: [T] => T => Numeric[T] ?=> T): ColumnVector = 
        import scalagrad.auto.forward.DeriverForwardPlan.given
        val df = ScalaGrad.derive(f[DualNumber[Double]]) // TODO this has to be lazy?
        DualColumnVector(
            v.value.map(f[Double]), 
            DeltaColumnVector.ElementWiseScale(v.value.map(df), v.delta)
        )

    override def elementWiseOpsM(v: Matrix, f: [T] => (x: T) => (Numeric[T]) ?=> T): Matrix =
        import scalagrad.auto.forward.DeriverForwardPlan.given
        val df = ScalaGrad.derive(f[DualNumber[Double]]) // TODO this has to be lazy?
        DualMatrix(
            v.value.map(f[Double]),
            DeltaMatrix.ElementWiseScale(v.value.map(df), v.delta)
        )

    override def sum(v: ColumnVector): Scalar = 
        DualScalar(breeze.linalg.sum(v.value), DeltaScalar.Sum(v.delta, v.value.length))

    override def length(v: ColumnVector): Int = v.value.size

    override def divideSS(s1: Scalar, s2: Scalar) = 
        def dDivide(u: Double, du: DeltaScalar[Double], v: Double, dv: DeltaScalar[Double]): DeltaScalar[Double] =
            DeltaScalar.Div(
                DeltaScalar.Sub(DeltaScalar.Scale(du, v), DeltaScalar.Scale(dv, u)), 
                (v * v)
            )
        DualScalar(s1.value / s2.value, dDivide(s1.value, s1.delta, s2.value, s2.delta))
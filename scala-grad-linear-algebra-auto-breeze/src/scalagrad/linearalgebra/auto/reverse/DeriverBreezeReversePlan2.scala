package scalagrad.linearalgebra.auto.reverse

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

import scalagrad.linearalgebra.auto.reverse.BreezeVectorAlgebraForDualDeltaDouble
import scalagrad.linearalgebra.auto.reverse.delta.*
import scalagrad.linearalgebra.auto.reverse.dual.*
import scalagrad.linearalgebra.auto.reverse.eval.EvalTotalOrder
import scalagrad.linearalgebra.auto.reverse.eval.EvalTotalOrder.Results

case class DeriverBreezeReversePlan2(private var index0: Int = 0):

    val ops = BreezeVectorAlgebraForDualDeltaDoubleTotalOrder(index0)

    def oneHotDenseVector(i: Int, size: Int): DenseVector[Double] =
        val res = DenseVector.zeros[Double](size)
        res(i) = 1.0
        res

    def zeroScalarM(key: Int) = DeltaScalar.Val[Double](key)
    def zeroColumnVectorM(key: Int) = DeltaColumnVector.Val[Double](key)
    def zeroRowVectorM(key: Int) = DeltaRowVector.Val[Double](key)
    def zeroMatrixM(key: Int) = DeltaMatrix.Val[Double](key)

    given scalar2Scalar: Deriver[
        ops.Scalar => ops.Scalar
    ] with

        override type dfT = Double => Double
        override def derive(f: fT): dfT = s =>
            val delta = f(DualDeltaScalar(s, zeroScalarM(0))).delta
            val result = EvalTotalOrder.evalScalar(1.0, delta)
            assert(result.scalars.size <= 1, s"result.scalars.size == ${result.scalars.size}")
            assert(result.matrices.size == 0, s"result.matrices.size == ${result.matrices.size}")
            assert(result.columnVectors.size == 0, s"result.columnVectors.size == ${result.columnVectors.size}")
            assert(result.rowVectors.size == 0, s"result.rowVectors.size == ${result.rowVectors.size}")
            result.scalars.get(0).getOrElse(0.0)

    given matrix2Scalar: Deriver[
        ops.Matrix => ops.Scalar
    ] with

        override type dfT = (
            DenseMatrix[Double],
        ) => (
            DenseMatrix[Double],
        )

        override def derive(f: fT): dfT = v =>
            val delta = f(DualDeltaMatrix(v, zeroMatrixM(0))).delta
            val result = EvalTotalOrder.evalScalar(1.0, delta)
            result.matrices(0)

    given rowVector2RowVector: Deriver[
        ops.RowVector => ops.RowVector
    ] with

        override type dfT = (
            Transpose[DenseVector[Double]],
        ) => (
            DenseMatrix[Double],
        )

        override def derive(f: fT): dfT = v =>
            val fRes = f(DualDeltaRowVector(v, zeroRowVectorM(0)))
            val inputLength = v.inner.length
            val outputLength = fRes.v.inner.length
            val res = DenseMatrix.zeros[Double](inputLength, outputLength)
            for (iRow <- 0 until outputLength) {
                val rv = oneHotDenseVector(iRow, outputLength).t
                val result = EvalTotalOrder.evalRowVector(rv, fRes.delta)
                assert(result.rowVectors(0).inner.length == inputLength)
                res(::, iRow) := result.rowVectors(0).t
            }
            res

    given vector2Scalar: Deriver[
        ops.ColumnVector => ops.Scalar
    ] with

        override type dfT = (
            DenseVector[Double],
        ) => (
            DenseVector[Double],
        )

        override def derive(f: fT): dfT = v =>
            val delta = f(DualDeltaColumnVector(v, zeroColumnVectorM(0))).delta
            val result = EvalTotalOrder.evalScalar(1.0, delta)
            result.columnVectors(0)

    given vectorMatrixVectorMatrix2Scalar: Deriver[
        (
            ops.ColumnVector,
            ops.Matrix,
            ops.ColumnVector,
            ops.Matrix
        ) =>  ops.Scalar
    ] with

        override type dfT = (
            DenseVector[Double],
            DenseMatrix[Double],
            DenseVector[Double],
            DenseMatrix[Double]
         ) => (
            DenseVector[Double],
            DenseMatrix[Double],
            DenseVector[Double],
            DenseMatrix[Double]
         )
        
        override def derive(f: fT): dfT =
            (v1, m1, v2, m2) =>
                val delta = f(
                    DualDeltaColumnVector(v1, zeroColumnVectorM(0)),
                    DualDeltaMatrix(m1, zeroMatrixM(1)),
                    DualDeltaColumnVector(v2, zeroColumnVectorM(2)),
                    DualDeltaMatrix(m2, zeroMatrixM(3))
                ).delta
                // import scalagrad.linearalgebra.auto.reverse.delta.visualize.DeltaToGraphviz
                // println(DeltaToGraphviz.countByType(delta))
                val result = EvalTotalOrder.evalScalar(1.0, delta)
                (result.columnVectors(0), result.matrices(1), result.columnVectors(2), result.matrices(3))

    given vectorMatrixScalarVector2Scalar: Deriver[
        (
            ops.ColumnVector,
            ops.Matrix,
            ops.Scalar,
            ops.ColumnVector
         ) => ops.Scalar
    ] with

        override type dfT = (
            DenseVector[Double],
            DenseMatrix[Double],
            Double,
            DenseVector[Double]
         ) => (
            DenseVector[Double],
            DenseMatrix[Double],
            Double,
            DenseVector[Double]
         )

        override def derive(f: fT): dfT = 
            (v1, m, s, v2) => {
                val delta = f(
                    DualDeltaColumnVector(v1, zeroColumnVectorM(0)),
                    DualDeltaMatrix(m, zeroMatrixM(1)),
                    DualDeltaScalar(s, zeroScalarM(2)),
                    DualDeltaColumnVector(v2, zeroColumnVectorM(3))
                ).delta
                val result = EvalTotalOrder.evalScalar(1.0, delta)
                (result.columnVectors(0), result.matrices(1), result.scalars(2), result.columnVectors(3))
            }
        
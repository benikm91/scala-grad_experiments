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
import scalagrad.linearalgebra.auto.reverse.eval.Eval
import scalagrad.linearalgebra.auto.reverse.eval.Eval.AccumulatedResult

object DeriverBreezeReversePlan:

    def oneHotDenseVector(i: Int, size: Int): DenseVector[Double] =
        val res = DenseVector.zeros[Double](size)
        res(i) = 1.0
        res

    def runDelta[P, D <: Deltas[P]](startId: DeltaId, deltaM: DeltaMonad[P, D]): Deltas[P] =
        def wrap(body: Deltas[P], stateEntry: (DeltaId, Deltas[P])): Deltas[P] = 
            body match
                case ds: DeltaScalar[P] => DeltaScalar.Let(stateEntry._1, stateEntry._2, ds)
                case dcv: DeltaColumnVector[P] => DeltaColumnVector.Let(stateEntry._1, stateEntry._2, dcv)
                case drv: DeltaRowVector[P] => DeltaRowVector.Let(stateEntry._1, stateEntry._2, drv)
                case dm: DeltaMatrix[P] => DeltaMatrix.Let(stateEntry._1, stateEntry._2, dm)
        val (finalState, result) = deltaM.run(DeltaState.start[P](startId))
        finalState.bindings.foldLeft(result)(wrap)
    
    def zeroScalarM(key: Int) = DeltaMonad[Double, DeltaScalar[Double]](state => (state, DeltaScalar.Val(key)))
    def zeroColumnVectorM(key: Int) = DeltaMonad[Double, DeltaColumnVector[Double]](state => (state, DeltaColumnVector.Val(key)))
    def zeroRowVectorM(key: Int) = DeltaMonad[Double, DeltaRowVector[Double]](state => (state, DeltaRowVector.Val(key)))
    def zeroMatrixM(key: Int) = DeltaMonad[Double, DeltaMatrix[Double]](state => (state, DeltaMatrix.Val(key)))

    given scalar2Scalar: Deriver[
        BreezeVectorAlgebraForDualDeltaDouble.Scalar => BreezeVectorAlgebraForDualDeltaDouble.Scalar
    ] with

        override type dfT = Double => Double
        override def derive(f: fT): dfT = s =>
            val delta = f(DualDeltaScalar(s, zeroScalarM(0))).delta
            val lala = runDelta(1, delta).asInstanceOf[DeltaScalar[Double]]
            val result = Eval.evalScalar(1.0, lala, Eval.AccumulatedResult.empty[Double])
            assert(result.scalars.size <= 1, s"result.scalars.size == ${result.scalars.size}")
            assert(result.matrices.size == 0, s"result.matrices.size == ${result.matrices.size}")
            assert(result.columnVectors.size == 0, s"result.columnVectors.size == ${result.columnVectors.size}")
            assert(result.rowVectors.size == 0, s"result.rowVectors.size == ${result.rowVectors.size}")
            result.scalars.get(0).getOrElse(0.0)

        def deriveResults(f: fT): Double => AccumulatedResult[Double] = s =>
            val delta = f(DualDeltaScalar(s, zeroScalarM(0))).delta
            val lala = runDelta(1, delta).asInstanceOf[DeltaScalar[Double]]
            val result = Eval.evalScalar(1.0, lala, Eval.AccumulatedResult.empty[Double])
            result

    given matrix2Scalar: Deriver[
        BreezeVectorAlgebraForDualDeltaDouble.Matrix => BreezeVectorAlgebraForDualDeltaDouble.Scalar
    ] with

        override type dfT = (
            DenseMatrix[Double],
        ) => (
            DenseMatrix[Double],
        )

        override def derive(f: fT): dfT = v =>
            val delta = f(DualDeltaMatrix(v, zeroMatrixM(0))).delta
            val lala = runDelta(1, delta).asInstanceOf[DeltaScalar[Double]]
            val result = Eval.evalScalar(1.0, lala, Eval.AccumulatedResult.empty[Double])
            result.matrices(0)

    given rowVector2RowVector: Deriver[
        BreezeVectorAlgebraForDualDeltaDouble.RowVector => BreezeVectorAlgebraForDualDeltaDouble.RowVector
    ] with

        override type dfT = (
            Transpose[DenseVector[Double]],
        ) => (
            DenseMatrix[Double],
        )

        override def derive(f: fT): dfT = v =>
            val fRes = f(DualDeltaRowVector(v, zeroRowVectorM(0)))
            val lala = runDelta(1, fRes.delta).asInstanceOf[DeltaRowVector[Double]]
            val inputLength = v.inner.length
            val outputLength = fRes.v.inner.length
            val res = DenseMatrix.zeros[Double](inputLength, outputLength)
            for (iRow <- 0 until outputLength) {
                val rv = oneHotDenseVector(iRow, outputLength).t
                val result = Eval.evalRowVector(rv, lala, Eval.AccumulatedResult.empty[Double])
                assert(result.rowVectors(0).inner.length == inputLength)
                res(::, iRow) := result.rowVectors(0).t
            }
            res

    given vector2Scalar: Deriver[
        BreezeVectorAlgebraForDualDeltaDouble.ColumnVector => BreezeVectorAlgebraForDualDeltaDouble.Scalar
    ] with

        override type dfT = (
            DenseVector[Double],
        ) => (
            DenseVector[Double],
        )

        override def derive(f: fT): dfT = v =>
            val delta = f(DualDeltaColumnVector(v, zeroColumnVectorM(0))).delta
            val lala = runDelta(1, delta).asInstanceOf[DeltaScalar[Double]]
            val result = Eval.evalScalar(1.0, lala, Eval.AccumulatedResult.empty[Double])
            result.columnVectors(0)

    given vectorMatrixVectorMatrix2Scalar: Deriver[
        (
            BreezeVectorAlgebraForDualDeltaDouble.ColumnVector,
            BreezeVectorAlgebraForDualDeltaDouble.Matrix,
            BreezeVectorAlgebraForDualDeltaDouble.ColumnVector,
            BreezeVectorAlgebraForDualDeltaDouble.Matrix
        ) =>  BreezeVectorAlgebraForDualDeltaDouble.Scalar
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
                val delta: DeltaMonad[Double, DeltaScalar[Double]] = f(
                    DualDeltaColumnVector(v1, zeroColumnVectorM(0)),
                    DualDeltaMatrix(m1, zeroMatrixM(1)),
                    DualDeltaColumnVector(v2, zeroColumnVectorM(2)),
                    DualDeltaMatrix(m2, zeroMatrixM(3))
                ).delta
                val lala = runDelta(4, delta).asInstanceOf[DeltaScalar[Double]]
                val result = Eval.evalScalar(1.0, lala, Eval.AccumulatedResult.empty[Double])
                (result.columnVectors(0), result.matrices(1), result.columnVectors(2), result.matrices(3))

    given vectorMatrixScalarVector2Scalar: Deriver[
        (
            BreezeVectorAlgebraForDualDeltaDouble.ColumnVector,
            BreezeVectorAlgebraForDualDeltaDouble.Matrix,
            BreezeVectorAlgebraForDualDeltaDouble.Scalar,
            BreezeVectorAlgebraForDualDeltaDouble.ColumnVector
         ) => BreezeVectorAlgebraForDualDeltaDouble.Scalar
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
                val delta: DeltaMonad[Double, DeltaScalar[Double]] = f(
                    DualDeltaColumnVector(v1, zeroColumnVectorM(0)),
                    DualDeltaMatrix(m, zeroMatrixM(1)),
                    DualDeltaScalar(s, zeroScalarM(2)),
                    DualDeltaColumnVector(v2, zeroColumnVectorM(3))
                ).delta
                val lala = runDelta(4, delta).asInstanceOf[DeltaScalar[Double]]
                val result = Eval.evalScalar(1.0, lala, Eval.AccumulatedResult.empty[Double])
                (result.columnVectors(0), result.matrices(1), result.scalars(2), result.columnVectors(3))
            }
        
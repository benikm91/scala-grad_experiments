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

object DeriverBreezeReversePlan:

    given vector2Scalar: Deriver[
        BreezeVectorAlgebraForDualDeltaDouble.ColumnVector => BreezeVectorAlgebraForDualDeltaDouble.Scalar
    ] with

        override type dfT = (
            DenseVector[Double],
        ) => (
            DenseVector[Double],
        )

        override def derive(f: fT): dfT = v =>
            val delta: DeltaMonad[Double, DeltaScalar[Double]] = f(
                DualDeltaColumnVector(v, zeroColumnVectorM(0)),
            ).delta
            val lala = runDelta(1, delta).asInstanceOf[DeltaScalar[Double]]
            val result = Eval.evalScalar(1.0, lala, Eval.AccumulatedResult.empty[Double])
            result.columnVectors(0)



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
        
    def runDelta[P](startId: DeltaId, deltaM: DeltaMonad[P, DeltaScalar[P]]): Deltas[P] =
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
            
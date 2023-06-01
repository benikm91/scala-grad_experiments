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

object DeriverBreezeReversePlan:

    given vector2Scalar: Deriver[
        BreezeVectorAlgebraForDualDeltaDouble.ColumnVector => BreezeVectorAlgebraForDualDeltaDouble.Scalar
    ] with

        override type dfT = DenseVector[Double] => DenseVector[Double]
        
        override def derive(f: fT): dfT =
            xs => {
                val keyXs = 0 until xs.length
                def toDelta(xs: DenseVector[Double]): DeltaScalar[Double] = 
                    val dual = DualColumnVector(xs, DeltaColumnVector.Val(0))
                    val res = f(dual)
                    res.delta
                val delta = toDelta(xs)
                val result = Eval.evalScalar(1.0, delta, Eval.AccumulatedResult.empty[Double])
                result.columnVectors(0)
            }

            
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
                val delta = f(
                    DualColumnVector(v1, DeltaColumnVector.Val(0)),
                    DualMatrix(m, DeltaMatrix.Val(0)),
                    DualScalar(s, DeltaScalar.Val(0)),
                    DualColumnVector(v2, DeltaColumnVector.Val(1))
                ).delta
                val result = Eval.evalScalar(1.0, delta, Eval.AccumulatedResult.empty[Double])
                (result.columnVectors(0), result.matrices(0), result.scalars(0), result.columnVectors(1))
            }
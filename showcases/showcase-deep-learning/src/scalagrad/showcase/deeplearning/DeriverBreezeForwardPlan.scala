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

object DeriverBreezeForwardPlan:

    given vector2Scalar: Deriver[
        BreezeVectorAlgebraForDualNumberDouble.ColumnVector => BreezeVectorAlgebraForDualNumberDouble.Scalar
    ] with

        override type dfT = DenseVector[Double] => DenseVector[Double]
        
        override def derive(f: fT): dfT =
            xs => 
                def oneHotDenseVector(i: Int, size: Int): DenseVector[Double] =
                    val res = DenseVector.zeros[Double](size)
                    res(i) = 1.0
                    res
                DenseVector((
                    for (i <- 0 until xs.length) 
                        yield f(
                            DualNumberColumnVector(xs, oneHotDenseVector(i, xs.length))
                        ).dv
                    ).toArray
                )
    
    given vectorMatrixScalarVector2Scalar: Deriver[
        (
            BreezeVectorAlgebraForDualNumberDouble.ColumnVector,
            BreezeVectorAlgebraForDualNumberDouble.Matrix,
            BreezeVectorAlgebraForDualNumberDouble.Scalar,
            BreezeVectorAlgebraForDualNumberDouble.ColumnVector
         ) => BreezeVectorAlgebraForDualNumberDouble.Scalar
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
            (v1, m, s, v2) => 
                def oneHotDenseVector(i: Int, size: Int): DenseVector[Double] =
                    val res = DenseVector.zeros[Double](size)
                    res(i) = 1.0
                    res
                def oneHotDenseMatrix(i: Int, mrows: Int, mcols: Int): DenseMatrix[Double] = 
                    val res = DenseMatrix.zeros[Double](mrows, mcols)
                    res(i / mrows, i % mcols) = 1.0
                    res
                val v1Dummy = DualNumberColumnVector(v1, DenseVector.zeros[Double](v1.length))
                val mDummy = DualNumberMatrix(m, DenseMatrix.zeros[Double](m.rows, m.cols))
                val sDummy = DualNumberScalar(s, 0.0) 
                val v2Dummy = DualNumberColumnVector(v2, DenseVector.zeros[Double](v2.length))

                val v1Res =
                    DenseVector((
                        for (i <- 0 until v1.length) 
                            yield f(
                                DualNumberColumnVector(v1, oneHotDenseVector(i, v1.length)),
                                mDummy,
                                sDummy,
                                v2Dummy
                            ).dv
                        ).toArray
                    )

                val mRes = 
                    DenseMatrix((
                        for (i <- 0 until m.rows * m.cols) 
                            yield f(
                                v1Dummy,
                                DualNumberMatrix(m, oneHotDenseMatrix(i, m.rows, m.cols)),
                                sDummy,
                                v2Dummy
                            ).dv
                        ).toArray
                    ).reshape(m.rows, m.cols)

                val sRes = f(v1Dummy, mDummy, DualNumberScalar(s, 1.0), v2Dummy).dv

                val v2Res =
                    DenseVector((
                        for (i <- 0 until v2.length) 
                            yield f(
                                v1Dummy,
                                mDummy,
                                sDummy,
                                DualNumberColumnVector(v2, oneHotDenseVector(i, v2.length))
                            ).dv
                        ).toArray
                    )

                (v1Res, mRes, sRes, v2Res)
package scalagrad.tensor.breeze.auto

import scalagrad.api.DeriverFromTo
import scalagrad.api.Deriver
import scalagrad.api.DeriverPlan
import scala.reflect.ClassTag
import scala.math.Fractional
import scala.runtime.Tuples
import breeze.linalg.DenseVector
import breeze.linalg.DenseMatrix
import breeze.storage.Zero
import breeze.linalg.support.LiteralRow

import scalagrad.auto.forward.dual.DualNumber

object DeriverTensorForwardPlan:

    // export DeriverPlan.given

    given vector[P: ClassTag : DualNumberDenseMatrixCapabilities : DualNumberDenseVectorCapabilities](using 
        frac: Fractional[P], 
        vector2Vector: DeriverFromTo[DualNumberDenseVector[P] => DualNumberDenseVector[P], DenseVector[P] => DenseMatrix[P]]
    ): Deriver[DualNumberDenseVector[P] => DualNumber[P]] with

        override type dfT = DenseVector[P] => DenseVector[P]
        
        override def derive(f: fT): dfT = xs =>
            val matrix = vector2Vector.derive(x => 
                val res = f(x)
                DualNumberDenseVector(DenseVector(res.value), DenseVector(res.derivative))
            )(xs)
            val vector: DenseVector[P] = matrix.toDenseVector
            assert(vector.length == matrix.rows, "Unexpected length of the resulting DenseVector")
            vector


    given vector2Vector[P : ClassTag : Zero : DualNumberDenseMatrixCapabilities : DualNumberDenseVectorCapabilities](using
        frac: Fractional[P]
    ): Deriver[DualNumberDenseVector[P] => DualNumberDenseVector[P]] with

        override type dfT = DenseVector[P] => DenseMatrix[P]
        
        override def derive(f: fT): dfT =
            def createOneHotVector(size: Int, index: Int): DenseVector[P] = {
                val vector = DenseVector.zeros[P](size)
                vector(index) = frac.one
                vector
            }
            xs => 
                val data: Vector[Vector[P]] = (
                    for (i <- 0 until xs.length) 
                        yield f(
                            DualNumberDenseVector(
                                xs,
                                createOneHotVector(xs.length, i)
                            )
                        ).derivative.toScalaVector
                ).toVector
                DenseMatrix.create(data.length, data.head.length, data.flatten.toArray)
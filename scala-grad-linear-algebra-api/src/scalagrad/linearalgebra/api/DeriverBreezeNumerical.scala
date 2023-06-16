package scalagrad.fractional.numerical

import scalagrad.api.Deriver
import scala.reflect.ClassTag
import scalagrad.api.linearalgebra.LinearAlgebraOps
import scalagrad.linearalgebra.api.BreezeVectorAlgebraForDouble
import breeze.linalg.DenseVector

/**
  * Implementation of Numerical Differentiation following the Difference quotient (also Newton quotient, Fermat's difference quotient) formula:
  * 
  * f'(x) = (f(x + e) - f(x)) / e
  */
object DeriverBreezeNumerical:

    val ops = BreezeVectorAlgebraForDouble

    trait DeriverP[P] extends Deriver[P => P] {
        override type dfT = P => P
    }

    trait DeriverPP[P] extends Deriver[(P, P) => P] {
        override type dfT = (P, P) => (P, P)
    }

    trait DeriverVP[P] extends Deriver[Vector[P] => P] {
        override type dfT = Vector[P] => Vector[P]
    }

    // Use central difference as default
    export CentralDifference.*

    object CentralDifference:

        def approxScalar2Scalar(e: ops.Scalar): Deriver[ops.Scalar => ops.Scalar] = 
            val halfE = e / 2
            new Deriver {
                override type dfT = ops.Scalar => ops.Scalar
                override def derive(f: ops.Scalar => ops.Scalar): dfT = (x) =>
                    (f(x + halfE) - f(x - halfE)) / e
            }

        def approxVector2Scalar(e: ops.Scalar): Deriver[ops.ColumnVector => ops.Scalar] = 
            val halfE = e / 2
            new Deriver {
                override type dfT = ops.ColumnVector => ops.ColumnVector
                override def derive(f: ops.ColumnVector => ops.Scalar): dfT = (xs) => 
                    DenseVector((
                        for (keyX <- 0 until xs.length) yield {
                            val xs2 = xs.copy
                            xs2(keyX) = xs2(keyX) + halfE
                            val xs3 = xs.copy
                            xs3(keyX) = xs3(keyX) - halfE
                            (f(xs2) - f(xs3)) / e
                        }
                    ).toArray)
            }

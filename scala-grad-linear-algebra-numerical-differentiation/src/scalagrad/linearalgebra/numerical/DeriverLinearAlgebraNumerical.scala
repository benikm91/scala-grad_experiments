package scalagrad.linearalgebra.numerical

import breeze.linalg.{DenseVector, DenseMatrix}
import scalagrad.api.Deriver
import scalagrad.api.linearalgebra.LinearAlgebraOps

/**
  * Implementation of Numerical Differentiation following the Difference quotient (also Newton quotient, Fermat's difference quotient) formula:
  * 
  * f'(x) = (f(x + e) - f(x)) / e
  */
object DeriverLinearAlgebraNumerical:

    trait DeriverVP extends Deriver[DenseVector[Double] => Double] {
        override type dfT = DenseVector[Double] => DenseVector[Double]
    }

    trait DeriverMP extends Deriver[DenseMatrix[Double] => Double] {
        override type dfT = DenseMatrix[Double] => DenseMatrix[Double]
    }

    // Use central difference as default
    export CentralDifference.*

    object CentralDifference:

        def approxMatrix(e: Double): DeriverMP = 
            val halfE = e / 2d
            new DeriverMP {
                override def derive(f: DenseMatrix[Double] => Double): dfT = (xs) => 
                    DenseMatrix.tabulate(xs.rows, xs.cols)((keyRow, keyCol) => 
                        val xs2 = xs.copy
                        xs2(keyRow, keyCol) = xs(keyRow, keyCol) + halfE
                        val xs3 = xs.copy
                        xs3(keyRow, keyCol) = xs(keyRow, keyCol) - halfE
                        (f(xs2) - f(xs3)) / e
                    )
            }

        def approxVector(e: Double): DeriverVP = 
            val halfE = e / 2d
            new DeriverVP {
                override def derive(f: DenseVector[Double] => Double): dfT = (xs) => 
                    DenseVector.tabulate(xs.size)((keyX) => 
                        val xs2 = xs.copy
                        xs2(keyX) = xs(keyX) + halfE
                        val xs3 = xs.copy
                        xs3(keyX) = xs(keyX) - halfE
                        (f(xs2) - f(xs3)) / e
                    )
            }
        
        // def approxMatrix(e: Double): DeriverMP[Double] =
        //     val halfE = e / 2d
        //     new DeriverMP[Double] {
        //         override type dfT = DenseMatrix[Double] => DenseMatrix[Double]
        //         override def derive(f: DenseMatrix[Double] => Double): dfT = (xs) => 
        //             DenseMatrix.tabulate(xs.rows, xs.cols)((i, j) => 
        //                 val xs2 = xs.copy
        //                 xs2(i, j) = xs(i, j) - halfE
        //                 val xs3 = xs.copy
        //                 xs3(i, j) = xs(i, j) + halfE
        //                 (f(xs2) - f(xs3)) / e
        //             )
        //     }
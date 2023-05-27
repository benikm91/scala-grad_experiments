package scalagrad.tensor.breeze.auto

import breeze.linalg._
import breeze.linalg.support.CanMapValues

/**
 * Things to consider:
 * 
 * - Sparse and Dense
 * - Vector and Matrix and Higher-order tensors
 * - Mutable and Immutable
 * - Operations (element-wise, dot product, reduction, broadcasting, slicing, etc.)
 * 
 * - Try to reuse breeze's implementation as much as possible...
 * - Think about generalizing API out of breeze for ScalaGrad...
 * 
 */

@main
def main =
    lolo()

def lala() =

    val m = DenseMatrix.zeros[Double](5, 5)
    val d1 = DenseVector(1.0, 2.0, 3.0)
    val d2 = DenseVector(2.0, 3.0, 4.0)

    val d3 = d1 + d2
    println(d3)

    // mutable
    m(4,::) := DenseVector(1.0, 2.0, 3.0, 4.0, 5.0).t
    d1(1 to 2) := .5

    // element-wise
    val m2 = m *:* 2.0

    // broadcasting
    m(::, *) + DenseVector(1.0, 2.0, 3.0, 4.0, 5.0)

    // dot product
    val d1d2: Double = d1.t * d2
    val d1m: DenseMatrix[Double] = d1 * m
    val md1: DenseVector[Double] = m * d1
    val ms1: DenseVector[Double] = m * SparseVector.zeros[Double](5)
    val sms1: SparseVector[Double] = CSCMatrix.zeros[Double](5, 5) * SparseVector.zeros[Double](5)

    // reduction
    val meanD1 = breeze.stats.mean(d1)
    val sumD1 = sum(d1)


def lolo() =
    
    val d1 = DualNumberDenseVector(
        DenseVector(1.0, 2.0, 3.0),
        DenseVector(0.0, 0.0, 0.0)
    )
    val d2 = DualNumberDenseVector(
        DenseVector(3.0, 4.0, 5.0),
        DenseVector(1.0, 0.0, 0.0)
    )
    
    println(d1 + d2)
    println(d1.dot(d2))
    // val d3 = d1 + d2
package scalagrad.tensor.breeze.auto

import breeze.linalg.DenseVector
import breeze.linalg.VectorLike
import scala.reflect.ClassTag
import breeze.storage.Zero
import breeze.linalg.support.CanCreateZerosLike
import breeze.linalg.DenseVectorDeps
import breeze.linalg.support.CanCopy
import breeze.linalg.support.CanMapValues
import breeze.linalg.support.ScalarOf
import breeze.linalg.support.CanZipMapValues
import scalagrad.api.Deriver
import scalagrad.api.ScalaGrad
import scalagrad.api.DeriverFromTo
import breeze.linalg.StorageVector


import breeze.linalg.DenseVector
import scalagrad.tensor.api.DualVector
import scalagrad.api.Dual
import scalagrad.auto.forward.dual.DualNumber
import breeze.linalg.operators.*
import breeze.linalg.DenseMatrix
import scalagrad.tensor.api.DualMatrix

import math.Fractional.Implicits.infixFractionalOps
import breeze.linalg.sum
import breeze.linalg.zipValues

// TODO Decide
// Maybe hard-coding value type to double is better as breeze capabilities are then in scope and must not be tracked...
case class DualNumberDenseVectorDouble(value: DenseVector[Double], derivative: DenseVector[Double])
    extends DualVector[Double, DenseVector, DenseMatrix, Double, DenseVector[Double], DenseMatrix[Double], DualNumber[Double], DualNumberDenseVectorDouble, DualNumberDenseMatrixDouble]:

    type DS = DualNumber[Double]
    type DV = DualNumberDenseVectorDouble
    type DM = DualNumberDenseMatrixDouble

    require(value.length == derivative.length)

    override def length: Int = value.length

    override def +(that: DV): DV = 
        DualNumberDenseVectorDouble(value + that.value, derivative + that.derivative)

    override def -(that: DV): DV = 
        DualNumberDenseVectorDouble(value - that.value, derivative - that.derivative)

    override def dot(that: DV): DS =
        def dDot(u: DenseVector[Double], du: DenseVector[Double], v: DenseVector[Double], dv: DenseVector[Double]): Double = 
            u.dot(dv) + du.dot(v)
        DualNumber(value.dot(that.value), dDot(value, derivative, that.value, that.derivative))

    def outerDot(that: DV): DM = 
        ???

    override def +(that: DS): DV = 
        DualNumberDenseVectorDouble(value + that.value, derivative + that.derivative)

    override def *(value: Double): DV =
        DualNumberDenseVectorDouble(this.value * value, this.derivative * value)

    override def *(that: DS): DV =
        def dTimes(u: DenseVector[Double], du: DenseVector[Double], v: Double, dv: Double): DenseVector[Double] = 
            u * dv + du * v
        DualNumberDenseVectorDouble(this.value * that.value, dTimes(this.value, this.derivative, that.value, that.derivative))

    override def map(f: Double => Double, df: Double => Double): DV =
        val mappedValue = DenseVector.zeros[Double](value.length)
        val mappedDerivative = DenseVector.zeros[Double](value.length)
        for (i <- 0 until value.length) {
            val v = value(i)
            val dv = derivative(i)
            
            val mappedDv = df(v) * dv
            mappedValue(i) = f(v)
            mappedDerivative(i) = mappedDv
        }
        DualNumberDenseVectorDouble(mappedValue, mappedDerivative)


    override def sum: DS = 
        DualNumber(breeze.linalg.sum(value), breeze.linalg.sum(derivative))

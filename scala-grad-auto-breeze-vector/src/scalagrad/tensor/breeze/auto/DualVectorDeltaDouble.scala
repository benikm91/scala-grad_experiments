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
import scalagrad.auto.reverse.dual.DualDelta
import breeze.linalg.operators.*
import breeze.linalg.DenseMatrix
import scalagrad.tensor.api.DualMatrix

import math.Fractional.Implicits.infixFractionalOps
import breeze.linalg.sum
import breeze.linalg.zipValues
import scalagrad.auto.reverse.dual.DualDelta
import scalagrad.auto.reverse.dual.delta.Delta
import scalagrad.auto.reverse.dual.delta.DeltaMonad

import scalagrad.auto.reverse.dual.delta.DeltaState

type AnyDelta[+P] = Delta[P] | DeltaVector[P] | DeltaMatrix[P]

case class DualDeltaDenseVectorDouble(value: DenseVector[Double], derivative: DeltaMonad[AnyDelta[Double], DeltaVector[Double]])
    extends DualVector[Double, DenseVector, DenseMatrix, Double, DeltaMonad[AnyDelta[Double], DeltaVector[Double]], DeltaMonad[AnyDelta[Double], DeltaMatrix[Double]], DualDelta[Double], DualDeltaDenseVectorDouble, DualNumberDenseMatrixDouble]:

    type D = DeltaMonad[AnyDelta[Double], DeltaVector[Double]]
    type DS = DualDelta[Double]
    type DV = DualDeltaDenseVectorDouble
    type DM = DualNumberDenseMatrixDouble

    override def length: Int = value.length

    def addD(d1: D, d2: D): D = 
        for {
            dx <- d1
            dy <- d2
            newId <- DualDelta.deltaLet(dx + dy)
        } yield DeltaVector.Val(newId)
    
    override def +(that: DV): DV = 
        DualDeltaDenseVectorDouble(value + that.value, addD(derivative, that.derivative))

    def subD(d1: D, d2: D): D =
        for {
            dx <- d1
            dy <- d2
            newId <- DualDelta.deltaLet(dx - dy)
        } yield DeltaVector.Val(newId)

    override def -(that: DV): DV = 
        DualDeltaDenseVectorDouble(value - that.value, subD(derivative, that.derivative))

    def dotD(u: DenseVector[Double], du: D, v: DenseVector[Double], dv: D): DeltaMonad[AnyDelta[Double], Delta[Double]] =
        for {
            dx <- du
            dy <- dv
            newId <- DualDelta.deltaLet(
                DeltaVector.Add(
                    DeltaVector.MulVector(dx, DeltaVector.ConstVector(v)),
                    DeltaVector.MulVector(dy, DeltaVector.ConstVector(u))
                )
            )
        } yield Delta.Val(newId)
    
    override def dot(that: DV): DS =
        DualDelta(value.dot(that.value), dotD(value, derivative, that.value, that.derivative))

    def outerDot(that: DV): DM = ???

    def addD(d1: D, d2: D): D = 
        for {
            dx <- d1
            dy <- d2
            newId <- deltaLet(dx + dy)
        } yield DeltaVector.Val(newId)
    
    override def +(that: DS): DV = 
        DualDeltaDenseVectorDouble(value + that.value, derivative + that.derivative)

    override def *(value: Double): DV =
        DualDeltaDenseVectorDouble(this.value * value, this.derivative * value)

    override def *(that: DS): DV =
        def dTimes(u: DenseVector[Double], du: DenseVector[Double], v: Double, dv: Double): DenseVector[Double] = 
            u * dv + du * v
        DualDeltaDenseVectorDouble(this.value * that.value, dTimes(this.value, this.derivative, that.value, that.derivative))

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
        DualDeltaDenseVectorDouble(mappedValue, mappedDerivative)

    override def sum: DS = 
        DualNumber(breeze.linalg.sum(value), breeze.linalg.sum(derivative))

object DualDeltaDenseVectorDouble:

    def valueOnly(value: DenseVector[Double]): DualDeltaDenseVectorDouble = 
        DualDeltaDenseVectorDouble(value, DenseVector.zeros[Double](value.length))

case class DualNumberDenseMatrixDouble(value: DenseMatrix[Double], derivative: DenseMatrix[Double])
    extends DualMatrix[Double, DenseVector, DenseMatrix, Double, DenseVector[Double], DenseMatrix[Double], DualDelta[Double], DualDeltaDenseVectorDouble, DualNumberDenseMatrixDouble]:

    type DS = DualDelta[Double]
    type DV = DualDeltaDenseVectorDouble
    type DM = DualNumberDenseMatrixDouble

    override def +(that: DS): DM = 
        DualNumberDenseMatrixDouble(value + that.value, derivative + that.derivative)

    override def +(that: DM): DM = 
        DualNumberDenseMatrixDouble(value + that.value, derivative + that.derivative)

    override def -(that: DM): DM = 
        DualNumberDenseMatrixDouble(value - that.value, derivative - that.derivative)

    override def *(that: DM): DM = 
        def dTimes(u: DenseMatrix[Double], du: DenseMatrix[Double], v: DenseMatrix[Double], dv: DenseMatrix[Double]): DenseMatrix[Double] = 
            u * dv + du * v
        DualNumberDenseMatrixDouble(value * that.value, dTimes(value, derivative, that.value, that.derivative))

    override def *(that: DV): DV = 
        def dTimes(u: DenseMatrix[Double], du: DenseMatrix[Double], v: DenseVector[Double], dv: DenseVector[Double]): DenseVector[Double] = 
            u * dv + du * v
        DualDeltaDenseVectorDouble(value * that.value, dTimes(value, derivative, that.value, that.derivative))

    override def *(that: DS): DM = 
        def dTimes(u: DenseMatrix[Double], du: DenseMatrix[Double], v: Double, dv: Double): DenseMatrix[Double] = 
            u * dv + du * v
        DualDeltaDenseMatrixDouble(value * that.value, dTimes(value, derivative, that.value, that.derivative))

object DualDeltaDenseMatrixDouble:

    def valueOnly(value: DenseMatrix[Double]): DualDeltaDenseMatrixDouble = 
        DualDeltaDenseMatrixDouble(value, DenseMatrix.zeros[Double](value.rows, value.cols))

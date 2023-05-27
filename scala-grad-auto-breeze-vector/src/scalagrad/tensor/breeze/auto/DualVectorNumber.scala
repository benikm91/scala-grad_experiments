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

class DualNumberDenseVectorCapabilities[T](
    ct: ClassTag[T],
    zero: Zero[T],
    ipadd: OpAdd.InPlaceImpl2[DenseVector[T] , T],
    a: OpAdd.Impl2[DenseVector[T], T, DenseVector[T]],
    b: OpAdd.Impl2[DenseVector[T], DenseVector[T], DenseVector[T]],
    c: OpSub.Impl2[DenseVector[T], DenseVector[T], DenseVector[T]],
    d: OpMulInner.Impl2[DenseVector[T], DenseVector[T], T],
    e: OpMulMatrix.Impl2[DenseVector[T], T, DenseVector[T]],
    f: CanMapValues[DenseVector[T], T, T, DenseVector[T]],
    g: sum.Impl[DenseVector[T], T],
):
    given ct2: ClassTag[T] = ct
    given zero2: Zero[T] = zero
    given ipadd2: OpAdd.InPlaceImpl2[DenseVector[T] , T] = ipadd
    given a2: OpAdd.Impl2[DenseVector[T], T, DenseVector[T]] = a
    given b2: OpAdd.Impl2[DenseVector[T], DenseVector[T], DenseVector[T]] = b
    given c2: OpSub.Impl2[DenseVector[T], DenseVector[T], DenseVector[T]] = c
    given d2: OpMulInner.Impl2[DenseVector[T], DenseVector[T], T] = d
    given e2: OpMulMatrix.Impl2[DenseVector[T], T, DenseVector[T]] = e
    given f2: CanMapValues[DenseVector[T], T, T, DenseVector[T]] = f
    given g2: sum.Impl[DenseVector[T], T] = g


object DualNumberDenseVectorCapabilities:

    given [T](using
        ct: ClassTag[T],
        zero: Zero[T],
        // DenseVector operations
        ipadd: OpAdd.InPlaceImpl2[DenseVector[T] , T],
        a: OpAdd.Impl2[DenseVector[T], T, DenseVector[T]],
        b: OpAdd.Impl2[DenseVector[T], DenseVector[T], DenseVector[T]],
        c: OpSub.Impl2[DenseVector[T], DenseVector[T], DenseVector[T]],
        d: OpMulInner.Impl2[DenseVector[T], DenseVector[T], T],
        e: OpMulMatrix.Impl2[DenseVector[T], T, DenseVector[T]],
        // DenseVector capabilities
        f: CanMapValues[DenseVector[T], T, T, DenseVector[T]],
        g: sum.Impl[DenseVector[T], T],
    ): DualNumberDenseVectorCapabilities[T] = 
        DualNumberDenseVectorCapabilities[T](ct, zero, ipadd, a, b, c, d, e, f, g)

class DualNumberDenseMatrixCapabilities[T](
    a: OpAdd.Impl2[DenseMatrix[T], DenseMatrix[T], DenseMatrix[T]],
    b: OpAdd.Impl2[DenseMatrix[T], T, DenseMatrix[T]],
    c: OpSub.Impl2[DenseMatrix[T], DenseMatrix[T], DenseMatrix[T]],
    // d: OpMulInner.Impl2[DenseMatrix[T], DenseMatrix[T], T],
    e: OpMulMatrix.Impl2[DenseMatrix[T], T, DenseMatrix[T]],
    f: OpMulMatrix.Impl2[DenseMatrix[T], DenseVector[T], DenseVector[T]],
    g: OpMulMatrix.Impl2[DenseMatrix[T], DenseMatrix[T], DenseMatrix[T]],
):
    given a2: OpAdd.Impl2[DenseMatrix[T], DenseMatrix[T], DenseMatrix[T]] = a
    given b2: OpAdd.Impl2[DenseMatrix[T], T, DenseMatrix[T]] = b
    given c2: OpSub.Impl2[DenseMatrix[T], DenseMatrix[T], DenseMatrix[T]] = c
    // given d2: OpMulInner.Impl2[DenseMatrix[T], DenseMatrix[T], T] = d
    given e2: OpMulMatrix.Impl2[DenseMatrix[T], T, DenseMatrix[T]] = e
    given f2: OpMulMatrix.Impl2[DenseMatrix[T], DenseVector[T], DenseVector[T]] = f
    given g2: OpMulMatrix.Impl2[DenseMatrix[T], DenseMatrix[T], DenseMatrix[T]] = g

object DualNumberDenseMatrixCapabilities:

    given [T](using
        // DenseMatrix operations
        a: OpAdd.Impl2[DenseMatrix[T], DenseMatrix[T], DenseMatrix[T]],
        b: OpAdd.Impl2[DenseMatrix[T], T, DenseMatrix[T]],
        c: OpSub.Impl2[DenseMatrix[T], DenseMatrix[T], DenseMatrix[T]],
        // d: OpMulInner.Impl2[DenseMatrix[T], DenseMatrix[T], T],
        e: OpMulMatrix.Impl2[DenseMatrix[T], T, DenseMatrix[T]],
        f: OpMulMatrix.Impl2[DenseMatrix[T], DenseVector[T], DenseVector[T]],
        g: OpMulMatrix.Impl2[DenseMatrix[T], DenseMatrix[T], DenseMatrix[T]],
    ): DualNumberDenseMatrixCapabilities[T] = 
        DualNumberDenseMatrixCapabilities[T](a, b, c, e, f, g)

case class DualNumberDenseVector[T](value: DenseVector[T], derivative: DenseVector[T])
    (using 
        vectorCapabilities: DualNumberDenseVectorCapabilities[T],
        frac: Fractional[T],
    )
    extends DualVector[T, DenseVector, DenseMatrix, T, DenseVector[T], DenseMatrix[T], DualNumber[T], DualNumberDenseVector[T], DualNumberDenseMatrix[T]]:

    import vectorCapabilities.given

    type DS = DualNumber[T]
    type DV = DualNumberDenseVector[T]
    type DM = DualNumberDenseMatrix[T]

    require(value.length == derivative.length)

    override def length: Int = value.length

    override def +(that: DV): DV = 
        DualNumberDenseVector(value + that.value, derivative + that.derivative)

    override def -(that: DV): DV = 
        DualNumberDenseVector(value - that.value, derivative - that.derivative)

    override def dot(that: DV): DS =
        def dDot(u: DenseVector[T], du: DenseVector[T], v: DenseVector[T], dv: DenseVector[T]): T = 
            u.dot(dv) + du.dot(v)
        DualNumber(value.dot(that.value), dDot(value, derivative, that.value, that.derivative))

    def outerDot(that: DV): DM = 
        ???

    override def +(that: DS): DV = 
        DualNumberDenseVector(value + that.value, derivative + that.derivative)

    override def *(value: T): DV =
        DualNumberDenseVector(this.value * value, this.derivative * value)

    override def *(that: DS): DV =
        def dTimes(u: DenseVector[T], du: DenseVector[T], v: T, dv: T): DenseVector[T] = 
            u * dv + du * v
        DualNumberDenseVector(this.value * that.value, dTimes(this.value, this.derivative, that.value, that.derivative))

    override def map(f: T => T, df: T => T): DV =
        val mappedValue = DenseVector.zeros[T](value.length)
        val mappedDerivative = DenseVector.zeros[T](value.length)
        for (i <- 0 until value.length) {
            val v = value(i)
            val dv = derivative(i)
            
            val mappedDv = df(v) * dv
            mappedValue(i) = f(v)
            mappedDerivative(i) = mappedDv
        }
        DualNumberDenseVector(mappedValue, mappedDerivative)


    override def sum: DS = 
        DualNumber(breeze.linalg.sum(value), breeze.linalg.sum(derivative))

object DualNumberDenseVector:

    def valueOnly[P : Zero : Fractional : ClassTag : DualNumberDenseVectorCapabilities : DualNumberDenseMatrixCapabilities](value: DenseVector[P]): DualNumberDenseVector[P] = 
        DualNumberDenseVector(value, DenseVector.zeros[P](value.length))

case class DualNumberDenseMatrix[T](value: DenseMatrix[T], derivative: DenseMatrix[T])
    (using 
        // DenseMatrix capabilities
        matrixCapabilities: DualNumberDenseMatrixCapabilities[T],
        vectorCapabilities: DualNumberDenseVectorCapabilities[T],
        // DualNumber capabilities needed
        frac: Fractional[T],
    )
    extends DualMatrix[T, DenseVector, DenseMatrix, T, DenseVector[T], DenseMatrix[T], DualNumber[T], DualNumberDenseVector[T], DualNumberDenseMatrix[T]]:

    import vectorCapabilities.given
    import matrixCapabilities.given

    type DS = DualNumber[T]
    type DV = DualNumberDenseVector[T]
    type DM = DualNumberDenseMatrix[T]

    override def +(that: DS): DM = 
        DualNumberDenseMatrix(value + that.value, derivative + that.derivative)

    override def +(that: DM): DM = 
        DualNumberDenseMatrix(value + that.value, derivative + that.derivative)

    override def -(that: DM): DM = 
        DualNumberDenseMatrix(value - that.value, derivative - that.derivative)

    override def *(that: DM): DM = 
        def dTimes(u: DenseMatrix[T], du: DenseMatrix[T], v: DenseMatrix[T], dv: DenseMatrix[T]): DenseMatrix[T] = 
            u * dv + du * v
        DualNumberDenseMatrix(value * that.value, dTimes(value, derivative, that.value, that.derivative))

    override def *(that: DV): DV = 
        def dTimes(u: DenseMatrix[T], du: DenseMatrix[T], v: DenseVector[T], dv: DenseVector[T]): DenseVector[T] = 
            u * dv + du * v
        DualNumberDenseVector(value * that.value, dTimes(value, derivative, that.value, that.derivative))

    override def *(that: DS): DM = 
        def dTimes(u: DenseMatrix[T], du: DenseMatrix[T], v: T, dv: T): DenseMatrix[T] = 
            u * dv + du * v
        DualNumberDenseMatrix(value * that.value, dTimes(value, derivative, that.value, that.derivative))

object DualNumberDenseMatrix:

    def valueOnly[P : Zero : Fractional : ClassTag : DualNumberDenseVectorCapabilities : DualNumberDenseMatrixCapabilities](value: DenseMatrix[P]): DualNumberDenseMatrix[P] = 
        DualNumberDenseMatrix(value, DenseMatrix.zeros[P](value.rows, value.cols))

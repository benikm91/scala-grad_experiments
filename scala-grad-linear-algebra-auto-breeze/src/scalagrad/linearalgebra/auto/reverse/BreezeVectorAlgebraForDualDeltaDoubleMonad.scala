package scalagrad.linearalgebra.auto.reverse

import scalagrad.api.ScalaGrad
import scalagrad.api.Dual
import scalagrad.api.linearalgebra.LinearAlgebraOps
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
import scalagrad.linearalgebra.api.BreezeVectorAlgebraForDualDouble
import scalagrad.linearalgebra.auto.reverse.dual.*
import scalagrad.linearalgebra.auto.reverse.delta.*
import scalagrad.linearalgebra.auto.forward.dual.{DualNumberScalar, DualNumberRowVector}
import scalagrad.linearalgebra.api.dual.DualScalar
import scalagrad.linearalgebra.auto.reverse.dual.DeltaState.deltaLet
import scalagrad.api.CreateDual

object BreezeVectorAlgebraForDualDeltaDoubleMonad extends LinearAlgebraOps:

    def liftMatrix(m: DenseMatrix[Double]): Matrix = 
        DeltaMonad(state => (state, DualDeltaMatrix[Double](m, DeltaMatrix.Zero(0.0))))

    val ops = BreezeVectorAlgebraForDualDeltaDouble

    override type Scalar = DeltaMonad[Double, DualDeltaScalar[Double]]
    override type ColumnVector = DeltaMonad[Double, DualDeltaColumnVector[Double]]
    override type RowVector = DeltaMonad[Double, DualDeltaRowVector[Double]]
    override type Matrix = DeltaMonad[Double, DualDeltaMatrix[Double]]

    override def plusMCV(m: Matrix, cv: ColumnVector): Matrix = 
        for {
            m <- m
            cv <- cv
            res = ops.plusMCV(m, cv)
            newId <- deltaLet(res.dv)
        } yield DualDeltaMatrix(res.v, DeltaMatrix.Val(newId))

    override def timesCVRV(cv1: ColumnVector, cv2: RowVector): Matrix = 
        for {
            cv1 <- cv1
            cv2 <- cv2
            res = ops.timesCVRV(cv1, cv2)
            newId <- deltaLet(res.dv)
        } yield DualDeltaMatrix(res.v, DeltaMatrix.Val(newId))

    override def minusMRV(m: Matrix, rv: RowVector): Matrix = 
        for {
            m <- m
            rv <- rv
            res = ops.minusMRV(m, rv)
            newId <- deltaLet(res.dv)
        } yield DualDeltaMatrix(res.v, DeltaMatrix.Val(newId))

    override def plusSS(s1: Scalar, s2: Scalar): Scalar = 
        for {
            s1 <- s1
            s2 <- s2
            res = ops.plusSS(s1, s2)
            newId <- deltaLet(res.dv)
        } yield DualDeltaScalar(res.v, DeltaScalar.Val(newId))

    override def timesMCV(m: Matrix, cv: ColumnVector): ColumnVector = 
        for {
            m <- m
            cv <- cv
            res = ops.timesMCV(m, cv)
            newId <- deltaLet(res.dv)
        } yield DualDeltaColumnVector(res.v, DeltaColumnVector.Val(newId))

    override def timesRVCV(rv: RowVector, cv: ColumnVector): Scalar = 
        for {
            rv <- rv
            cv <- cv
            res = ops.timesRVCV(rv, cv)
            newId <- deltaLet(res.dv)
        } yield DualDeltaScalar(res.v, DeltaScalar.Val(newId))

    override def plusMM(m1: Matrix, m2: Matrix): Matrix = 
        for {
            m1 <- m1
            m2 <- m2
            res = ops.plusMM(m1, m2)
            newId <- deltaLet(res.dv)
        } yield DualDeltaMatrix(res.v, DeltaMatrix.Val(newId))

    override def minusMS(m: Matrix, s: Scalar): Matrix = 
        for {
            m <- m
            s <- s
            res = ops.minusMS(m, s)
            newId <- deltaLet(res.dv)
        } yield DualDeltaMatrix(res.v, DeltaMatrix.Val(newId))

    override def minusCVS(cv: ColumnVector, s: Scalar): ColumnVector =
        for {
            cv <- cv
            s <- s
            res = ops.minusCVS(cv, s)
            newId <- deltaLet(res.dv)
        } yield DualDeltaColumnVector(res.v, DeltaColumnVector.Val(newId))

    override def plusMS(m: Matrix, s: Scalar): Matrix = 
        for {
            m <- m
            s <- s
            res = ops.plusMS(m, s)
            newId <- deltaLet(res.dv)
        } yield DualDeltaMatrix(res.v, DeltaMatrix.Val(newId))

    override def stackRows(rvs: RowVector*): Matrix = ???

    override def columnWiseOpsM(m: Matrix, cv2cv: ColumnVector => ColumnVector): Matrix = ???

    override def timesMM(m1: Matrix, m2: Matrix): Matrix = 
        for {
            m1 <- m1
            m2 <- m2
            res = ops.timesMM(m1, m2)
            newId <- deltaLet(res.dv)
        } yield DualDeltaMatrix(res.v, DeltaMatrix.Val(newId))

    override def sumM(v: Matrix): Scalar = 
        for {
            v <- v
            res = ops.sumM(v)
            newId <- deltaLet(res.dv)
        } yield DualDeltaScalar(res.v, DeltaScalar.Val(newId))

    override def divideRVS(rv: RowVector, s: Scalar): RowVector = 
        for {
            rv <- rv
            s <- s
            res = ops.divideRVS(rv, s)
            newId <- deltaLet(res.dv)
        } yield DualDeltaRowVector(res.v, DeltaRowVector.Val(newId))

    override def sumCV(cv: ColumnVector): Scalar = 
        for {
            cv <- cv
            res = ops.sumCV(cv)
            newId <- deltaLet(res.dv)
        } yield DualDeltaScalar(res.v, DeltaScalar.Val(newId))

    override def liftToScalar(c: Double): Scalar = ???

    override def liftToScalar(c: Int): Scalar = ???

    override def foldLeftCV(s: Scalar)(cv: ColumnVector)(f: (Scalar, Scalar) => Scalar): Scalar = ???

    override def divideMS(m: Matrix, s: Scalar): Matrix = 
        for {
            m <- m
            s <- s
            res = ops.divideMS(m, s)
            newId <- deltaLet(res.dv)
        } yield DualDeltaMatrix(res.v, DeltaMatrix.Val(newId))

    override def plusRVRV(rv1: RowVector, rv2: RowVector): RowVector = 
        for {
            rv1 <- rv1
            rv2 <- rv2
            res = ops.plusRVRV(rv1, rv2)
            newId <- deltaLet(res.dv)
        } yield DualDeltaRowVector(res.v, DeltaRowVector.Val(newId))

    override def timesCVS(cv: ColumnVector, s: Scalar): ColumnVector = 
        for {
            cv <- cv
            s <- s
            res = ops.timesCVS(cv, s)
            newId <- deltaLet(res.dv)
        } yield DualDeltaColumnVector(res.v, DeltaColumnVector.Val(newId))

    override def determinant(m: Matrix): Scalar = ???

    override def rowWiseOpsM(m: Matrix, rv2rv: RowVector => RowVector): Matrix = ???

    override def divideCVS(cv: ColumnVector, s: Scalar): ColumnVector = 
        for {
            cv <- cv
            s <- s
            res = ops.divideCVS(cv, s)
            newId <- deltaLet(res.dv)
        } yield DualDeltaColumnVector(res.v, DeltaColumnVector.Val(newId))

    override def timesRVS(rv: RowVector, s: Scalar): RowVector = 
        for {
            rv <- rv
            s <- s
            res = ops.timesRVS(rv, s)
            newId <- deltaLet(res.dv)
        } yield DualDeltaRowVector(res.v, DeltaRowVector.Val(newId))

    override def elementWiseTimesCVCV(cv1: ColumnVector, cv2: ColumnVector): ColumnVector =
        for {
            cv1 <- cv1
            cv2 <- cv2
            res = ops.elementWiseTimesCVCV(cv1, cv2)
            newId <- deltaLet(res.dv)
        } yield DualDeltaColumnVector(res.v, DeltaColumnVector.Val(newId))

    override def minusCVCV(cv1: ColumnVector, cv2: ColumnVector): ColumnVector = 
        for {
            cv1 <- cv1
            cv2 <- cv2
            res = ops.minusCVCV(cv1, cv2)
            newId <- deltaLet(res.dv)
        } yield DualDeltaColumnVector(res.v, DeltaColumnVector.Val(newId))

    override def elementWiseOpsRV(rv: RowVector, s2s: Scalar => Scalar): RowVector = ???

    override def minusSS(s1: Scalar, s2: Scalar): Scalar = 
        for {
            s1 <- s1
            s2 <- s2
            res = ops.minusSS(s1, s2)
            newId <- deltaLet(res.dv)
        } yield DualDeltaScalar(res.v, DeltaScalar.Val(newId))

    override def elementWiseOpsM(m: Matrix, s2s: Scalar => Scalar): Matrix = ???

    override def plusRVS(v: RowVector, s: Scalar): RowVector = 
        for {
            v <- v
            s <- s
            res = ops.plusRVS(v, s)
            newId <- deltaLet(res.dv)
        } yield DualDeltaRowVector(res.v, DeltaRowVector.Val(newId))

    override def lengthColumnVector(cv: ColumnVector): Int = ???

    override def lengthRowVector(rv: RowVector): Int = ???

    override def scalarToDouble(s: Scalar): Double = ???

    override def minusMCV(m: Matrix, cv: ColumnVector): Matrix = 
        for {
            m <- m
            cv <- cv
            res = ops.minusMCV(m, cv)
            newId <- deltaLet(res.dv)
        } yield DualDeltaMatrix(res.v, DeltaMatrix.Val(newId))

    override def transposeRowVector(rv: RowVector): ColumnVector = 
        for {
            rv <- rv
            res = ops.transposeRowVector(rv)
            newId <- deltaLet(res.dv)
        } yield DualDeltaColumnVector(res.v, DeltaColumnVector.Val(newId))

    override def elementAtM(m: Matrix, rowI: Int, columnJ: Int): Scalar = 
        for {
            m <- m
            res = ops.elementAtM(m, rowI, columnJ)
            newId <- deltaLet(res.dv)
        } yield DualDeltaScalar(res.v, DeltaScalar.Val(newId))

    override def timesRVM(rv: RowVector, m: Matrix): RowVector = 
        for {
            rv <- rv
            m <- m
            res = ops.timesRVM(rv, m)
            newId <- deltaLet(res.dv)
        } yield DualDeltaRowVector(res.v, DeltaRowVector.Val(newId))

    override def nCols(m: Matrix): Int = ???

    override def plusCVCV(cv1: ColumnVector, cv2: ColumnVector): ColumnVector = 
        for {
            cv1 <- cv1
            cv2 <- cv2
            res = ops.plusCVCV(cv1, cv2)
            newId <- deltaLet(res.dv)
        } yield DualDeltaColumnVector(res.v, DeltaColumnVector.Val(newId))

    override def elementWiseOpsCV(cv: ColumnVector, s2s: Scalar => Scalar): ColumnVector = ???

    override def nRows(m: Matrix): Int = ???

    override def minusRVS(rv: RowVector, s: Scalar): RowVector = 
        for {
            rv <- rv
            s <- s
            res = ops.minusRVS(rv, s)
            newId <- deltaLet(res.dv)
        } yield DualDeltaRowVector(res.v, DeltaRowVector.Val(newId))

    override def transposeColumVector(cv: ColumnVector): RowVector = 
        for {
            cv <- cv
            res = ops.transposeColumVector(cv)
            newId <- deltaLet(res.dv)
        } yield DualDeltaRowVector(res.v, DeltaRowVector.Val(newId))

    override def elementAtCV(cv: ColumnVector, i: Int): Scalar = 
        for {
            cv <- cv
            res = ops.elementAtCV(cv, i)
            newId <- deltaLet(res.dv)
        } yield DualDeltaScalar(res.v, DeltaScalar.Val(newId))

    override def minusRVRV(rv1: RowVector, rv2: RowVector): RowVector = 
        for {
            rv1 <- rv1
            rv2 <- rv2
            res = ops.minusRVRV(rv1, rv2)
            newId <- deltaLet(res.dv)
        } yield DualDeltaRowVector(res.v, DeltaRowVector.Val(newId))

    override def timesSS(s1: Scalar, s2: Scalar): Scalar = 
        for {
            s1 <- s1
            s2 <- s2
            res = ops.timesSS(s1, s2)
            newId <- deltaLet(res.dv)
        } yield DualDeltaScalar(res.v, DeltaScalar.Val(newId))

    override def minusMM(m1: Matrix, m2: Matrix): Matrix = 
        for {
            m1 <- m1
            m2 <- m2
            res = ops.minusMM(m1, m2)
            newId <- deltaLet(res.dv)
        } yield DualDeltaMatrix(res.v, DeltaMatrix.Val(newId))

    override def plusCVS(cv: ColumnVector, s: Scalar): ColumnVector = 
        for {
            cv <- cv
            s <- s
            res = ops.plusCVS(cv, s)
            newId <- deltaLet(res.dv)
        } yield DualDeltaColumnVector(res.v, DeltaColumnVector.Val(newId))

    override def plusMRV(m: Matrix, rv: RowVector): Matrix = 
        for {
            m <- m
            rv <- rv
            res = ops.plusMRV(m, rv)
            newId <- deltaLet(res.dv)
        } yield DualDeltaMatrix(res.v, DeltaMatrix.Val(newId))

    override def inverse(m: Matrix): Matrix = ???

    override def rowAtM(m: Matrix, rowI: Int): RowVector = ???

    override def timesMS(m: Matrix, s: Scalar): Matrix = 
        for {
            m <- m
            s <- s
            res = ops.timesMS(m, s)
            newId <- deltaLet(res.dv)
        } yield DualDeltaMatrix(res.v, DeltaMatrix.Val(newId))

    override def transpose(m: Matrix): Matrix = 
        for {
            m <- m
            res = ops.transpose(m)
            newId <- deltaLet(res.dv)
        } yield DualDeltaMatrix(res.v, DeltaMatrix.Val(newId))

    override def divideSS(s1: Scalar, s2: Scalar): Scalar = 
        for {
            s1 <- s1
            s2 <- s2
            res = ops.divideSS(s1, s2)
            newId <- deltaLet(res.dv)
        } yield DualDeltaScalar(res.v, DeltaScalar.Val(newId))

    override def elementWiseTimesMM(m1: Matrix, m2: Matrix): Matrix = 
        for {
            m1 <- m1
            m2 <- m2
            res = ops.elementWiseTimesMM(m1, m2)
            newId <- deltaLet(res.dv)
        } yield DualDeltaMatrix(res.v, DeltaMatrix.Val(newId))
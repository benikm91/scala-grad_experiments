package scalagrad.api.linearalgebra

import scala.annotation.targetName
import scala.math.Fractional.Implicits._

class ScalaLinearAlgebraOpsFor[P: Fractional]() extends LinearAlgebraOps:

    override type Scalar = P
    override type ColumnVector = Vector[P]
    override type RowVector = Vector[P]
    override type Matrix = Vector[Vector[P]]
    
    def cofactor(m: Matrix, i: Int, j: Int): Scalar = 
        def minorMatrix(m: Matrix, i: Int, j: Int): Matrix = 
            m.zipWithIndex.filter((_, idx) => idx != i).map(row => row._1.zipWithIndex.filter((_, idx) => idx != j).map(_._1))
        val minor = minorMatrix(m, i, j)
        val sign = if (i + j % 2 == 0) 1 else -1
        liftToScalar(sign) * determinant(minor)

    override def inverse(m: Matrix): Matrix = 
        val det = determinant(m)
        val n = nRows(m)
        val cofactors = (0 until n).map(i => (0 until n).map(j => cofactor(m, i, j)).toVector).toVector
        transpose(cofactors).map(row => row.map(a => a / det))

    override def determinant(m: Matrix): Scalar =
        if (nRows(m) == 1) m.head.head
        else m.head.zipWithIndex.map((a, idx) => a * cofactor(m, 0, idx)).sum

    override def nRows(m: Matrix): Int = m.length
    override def nCols(m: Matrix): Int = m.head.length
    override def lengthColumnVector(v: ColumnVector): Int = v.length
    override def lengthRowVector(v: RowVector): Int = v.length

    override def liftToScalar(d: Int): Scalar = summon[Fractional[Scalar]].fromInt(d)
    override def liftToScalar(d: Double): Scalar = ???

    override def scalarToDouble(s: Scalar) = summon[Fractional[Scalar]].toDouble(s)

    override def transpose(m: Matrix): Matrix = m.transpose

    override def transposeColumVector(v: ColumnVector): RowVector = v 

    override def transposeRowVector(v: RowVector): ColumnVector = v
    
    override def timesMM(m1: Matrix, m2: Matrix): Matrix = 
        m1.map(row => m2.transpose.map(col => row.zip(col).map((a, b) => a * b).sum))

    override def timesMCV(m: Matrix, v: ColumnVector): ColumnVector = 
        m.map(row => row.zip(v).map((a, b) => a * b).sum)

    override def timesMS(m: Matrix, s: Scalar): Matrix = 
        m.map(row => row.map(a => a * s))
    
    override def timesCVRV(v1: ColumnVector, v2: RowVector): Matrix = 
        v1.map(a => v2.map(b => a * b))

    override def timesCVS(v: ColumnVector, s: Scalar): ColumnVector = 
        v.map(a => a * s)

    override def timesRVM(v: RowVector, m: Matrix): RowVector = 
        m.transpose.map(row => row.zip(v).map((a, b) => a * b).sum)

    override def timesRVCV(v1: RowVector, v2: ColumnVector): Scalar = 
        v1.zip(v2).map((a, b) => a * b).sum
    
    override def timesRVS(v: RowVector, s: Scalar): RowVector = 
        v.map(a => a * s)

    override def timesSM(s: Scalar, m: Matrix): Matrix = timesMS(m, s)
    override def timesSCV(s: Scalar, v: ColumnVector): ColumnVector = timesCVS(v, s)
    override def timesSRV(s: Scalar, v: RowVector): RowVector = timesRVS(v, s)
    override def timesSS(s1: Scalar, s2: Scalar): Scalar = summon[Fractional[Scalar]].times(s1, s2)

    override def plusMM(m1: Matrix, m2: Matrix): Matrix =
        m1.zip(m2).map((row1, row2) => row1.zip(row2).map((a, b) => a + b))

    override def plusMCV(m: Matrix, v: ColumnVector): Matrix = 
        m.zip(v).map((row, s) => row.map(a => a + s))

    override def plusMRV(m: Matrix, v: RowVector): Matrix = 
        m.zip(v).map((row, s) => row.map(a => a + s))

    override def plusMS(m: Matrix, s: Scalar): Matrix = 
        m.map(row => row.map(a => a + s))

    override def plusCVM(v: ColumnVector, m: Matrix): Matrix = plusMCV(m, v)
    
    override def plusCVCV(v1: ColumnVector, v2: ColumnVector): ColumnVector =
        v1.zip(v2).map((a, b) => a + b)
    
    override def plusCVS(v: ColumnVector, s: Scalar): ColumnVector =
        v.map(a => a + s)

    override def plusRVM(v: RowVector, m: Matrix): Matrix = plusMRV(m, v)
    override def plusRVRV(v1: RowVector, v2: RowVector): RowVector =
        v1.zip(v2).map((a, b) => a + b)
    override def plusRVS(v: RowVector, s: Scalar): RowVector = 
        v.map(a => a + s)
        
    override def plusSM(s: Scalar, m: Matrix): Matrix = plusMS(m, s)
    override def plusSCV(s: Scalar, v: ColumnVector): ColumnVector = plusCVS(v, s) 
    override def plusSRV(s: Scalar, v: RowVector): RowVector = plusRVS(v, s)
    override def plusSS(s1: Scalar, s2: Scalar): Scalar =
        summon[Fractional[Scalar]].plus(s1, s2)

    override def minusMM(m1: Matrix, m2: Matrix): Matrix =
        m1.zip(m2).map((row1, row2) => row1.zip(row2).map((a, b) => a - b))
    override def minusMCV(m: Matrix, v: ColumnVector): Matrix =
        m.zip(v).map((row, s) => row.map(a => a - s))
    override def minusMRV(m: Matrix, v: RowVector): Matrix =
        m.zip(v).map((row, s) => row.map(a => a - s)) 
    override def minusMS(m: Matrix, s: Scalar): Matrix = 
        m.map(row => row.map(a => a - s))

    override def minusCVM(v: ColumnVector, m: Matrix): Matrix = minusMCV(m, v) 
    override def minusCVCV(v1: ColumnVector, v2: ColumnVector): ColumnVector = 
        v1.zip(v2).map((a, b) => a - b)
    override def minusCVS(v: ColumnVector, s: Scalar): ColumnVector =
        v.map(a => a - s)
    
    override def minusRVM(v: RowVector, m: Matrix): Matrix = minusMRV(m, v)
    override def minusRVRV(v1: RowVector, v2: RowVector): RowVector = 
        v1.zip(v2).map((a, b) => a - b)
    override def minusRVS(v: RowVector, s: Scalar): RowVector = 
        v.map(a => a - s)

    override def minusSM(s: Scalar, m: Matrix): Matrix = minusMS(m, s)
    override def minusSCV(s: Scalar, v: ColumnVector): ColumnVector = minusCVS(v, s) 
    override def minusSRV(s: Scalar, v: RowVector): RowVector = minusRVS(v, s)
    override def minusSS(s1: Scalar, s2: Scalar): Scalar =
        summon[Fractional[Scalar]].minus(s1, s2)

    override def divideMS(m: Matrix, s: Scalar): Matrix = 
        m.map(row => row.map(a => a / s))
    
    override def divideCVS(v: ColumnVector, s: Scalar): ColumnVector =
        v.map(a => a / s)
    
    override def divideRVS(v: RowVector, s: Scalar): RowVector =
        v.map(a => a / s)

    override def divideSM(s: Scalar, m: Matrix): Matrix = divideMS(m, s)
    override def divideSCV(s: Scalar, v: ColumnVector): ColumnVector = divideCVS(v, s) 
    override def divideSRV(s: Scalar, v: RowVector): RowVector = divideRVS(v, s)
    override def divideSS(s1: Scalar, s2: Scalar): Scalar =
        summon[Fractional[Scalar]].div(s1, s2)
        
    override def foldLeftCV(s: Scalar)(v: ColumnVector)(f: (Scalar, Scalar) => Scalar): Scalar =
        v.foldLeft(s)((a, b) => f(a, b))

    override def sumCV(v: ColumnVector): Scalar = v.sum

    override def sumM(v: Matrix): Scalar = v.flatten.sum

    override def elementWiseTimesMM(m1: Matrix, m2: Matrix): Matrix = 
        m1.zip(m2).map((row1, row2) => row1.zip(row2).map((a, b) => a * b))    

    override def elementWiseTimesCVCV(v1: ColumnVector, v2: ColumnVector): ColumnVector = 
        v1.zip(v2).map((a, b) => a * b)

    override def elementWiseOpsM(v: Matrix, f: Scalar => Scalar): Matrix = 
        v.map(row => row.map(a => f(a)))

    override def columnWiseOpsM(m: Matrix, f: ColumnVector => ColumnVector): Matrix = 
        m.transpose.map(col => f(col).toVector)
    
    override def rowWiseOpsM(m: Matrix, f: RowVector => RowVector): Matrix =
        m.map(row => f(row).toVector)

    override def elementWiseOpsCV(v: ColumnVector, f: Scalar => Scalar): ColumnVector =
        v.map(a => f(a))

    override def elementWiseOpsRV(v: RowVector, f: Scalar => Scalar): RowVector = 
        v.map(a => f(a))

    override def elementAtM(m: Matrix, rowI: Int, columnJ: Int): Scalar = m(rowI)(columnJ)
    def rowAtM(m: Matrix, rowI: Int): RowVector = m(rowI)

    override def elementAtCV(v: ColumnVector, i: Int): Scalar = v(i)

    def stackRows(rows: RowVector*): Matrix = rows.toVector

package scalagrad.api.linearalgebra

import scala.annotation.targetName
import scala.math.Fractional.Implicits._


trait LinearAlgebraOps:

    type Scalar
    type ColumnVector
    type RowVector
    type Matrix

    def nRows(m: Matrix): Int
    def nCols(m: Matrix): Int

    extension (m: Matrix)
        @targetName("nRows_Op")
        def rows: Int = nRows(m)
        @targetName("nCols_Op")
        def cols: Int = nCols(m)

    def lengthColumnVector(v: ColumnVector): Int

    extension (v: ColumnVector)
        @targetName("lengthColumnVector_Op")
        def length: Int = lengthColumnVector(v)

    def lengthRowVector(v: RowVector): Int

    extension (v: RowVector)
        @targetName("lengthRowVector_Op")
        def length: Int = lengthRowVector(v)

    def liftToScalar(d: Int): Scalar

    extension (d: Int)
        def toScalar: Scalar = liftToScalar(d)

    def liftToScalar(d: Double): Scalar
    
    extension (d: Double)
        def toScalar: Scalar = liftToScalar(d)

    def scalarToDouble(d: Scalar): Double

    extension (d: Scalar)
        def toDouble: Double = scalarToDouble(d)

    def inverse(m: Matrix): Matrix

    extension (m: Matrix)
        @targetName("inverse_Op")
        def inv: Matrix = inverse(m)

    def determinant(m: Matrix): Scalar

    extension (m: Matrix)
        @targetName("determinant_Op")
        def det: Scalar = determinant(m)

    def transpose(m: Matrix): Matrix

    extension (m: Matrix)
        @targetName("transpose_Op")
        def T: Matrix = transpose(m)

    def transposeColumVector(v: ColumnVector): RowVector

    extension (v: ColumnVector)
        @targetName("transposeColumnVector_Op")
        def T: RowVector = transposeColumVector(v)

    def transposeRowVector(v: RowVector): ColumnVector
    
    extension (v: RowVector)
        @targetName("transposeRowVector_Op")
        def T: ColumnVector = transposeRowVector(v)

    def timesMM(m1: Matrix, m2: Matrix): Matrix
    def timesMCV(m: Matrix, v: ColumnVector): ColumnVector
    def timesMS(m: Matrix, s: Scalar): Matrix
    
    extension (m1: Matrix)
        @targetName("timesMM_Op")
        def *(m2: Matrix): Matrix = timesMM(m1, m2)
        @targetName("timesMCV_Op")
        def *(v: ColumnVector): ColumnVector = timesMCV(m1, v)
        @targetName("timesMS_Op")
        def *(s: Scalar): Matrix = timesMS(m1, s)
        
    def timesCVRV(v1: ColumnVector, v2: RowVector): Matrix
    def timesCVS(v: ColumnVector, s: Scalar): ColumnVector

    extension (v: ColumnVector)
        @targetName("timesCVRV_Op")
        def *(v2: RowVector): Matrix = timesCVRV(v, v2)
        @targetName("timesCVS_Op")
        def *(s: Scalar): ColumnVector = timesCVS(v, s)

    def timesRVM(v: RowVector, m: Matrix): RowVector
    def timesRVCV(v1: RowVector, v2: ColumnVector): Scalar
    def timesRVS(v: RowVector, s: Scalar): RowVector

    extension (v: RowVector)
        @targetName("timesRVM_Op")
        def *(m: Matrix): RowVector = timesRVM(v, m)
        @targetName("timesRVCV_Op")
        def *(v2: ColumnVector): Scalar = timesRVCV(v, v2)
        @targetName("timesRVS_Op")
        def *(s: Scalar): RowVector = timesRVS(v, s)

    def timesSM(s: Scalar, m: Matrix): Matrix = timesMS(m, s)
    def timesSCV(s: Scalar, v: ColumnVector): ColumnVector = timesCVS(v, s)
    def timesSRV(s: Scalar, v: RowVector): RowVector = timesRVS(v, s)
    def timesSS(s1: Scalar, s2: Scalar): Scalar

    extension (s: Scalar)
        @targetName("timesSM_Op")
        def *(m: Matrix): Matrix = timesSM(s, m)
        @targetName("timesSCV_Op")
        def *(v: ColumnVector): ColumnVector = timesSCV(s, v)
        @targetName("timesSRV_Op")
        def *(v: RowVector): RowVector = timesSRV(s, v)
        @targetName("timesSS_Op")
        def *(s2: Scalar): Scalar = timesSS(s, s2)

    def plusMM(m1: Matrix, m2: Matrix): Matrix
    def plusMCV(m: Matrix, v: ColumnVector): Matrix
    def plusMRV(m: Matrix, v: RowVector): Matrix
    def plusMS(m: Matrix, s: Scalar): Matrix

    extension (m1: Matrix)
        @targetName("plusMM_Op")
        def +(m2: Matrix): Matrix = plusMM(m1, m2)
        @targetName("plusMCV_Op")
        def +(v: ColumnVector): Matrix = plusMCV(m1, v)
        @targetName("plusMRV_Op")
        def +(v: RowVector): Matrix = plusMRV(m1, v)
        @targetName("plusMS_Op")
        def +(s: Scalar): Matrix = plusMS(m1, s)
        
    def plusCVM(v: ColumnVector, m: Matrix): Matrix = plusMCV(m, v)
    def plusCVCV(v1: ColumnVector, v2: ColumnVector): ColumnVector
    def plusCVS(v: ColumnVector, s: Scalar): ColumnVector

    extension (v: ColumnVector)
        @targetName("plusCVM_Op")
        def +(m: Matrix): Matrix = plusCVM(v, m)
        @targetName("plusCVCV_Op")
        def +(v2: ColumnVector): ColumnVector = plusCVCV(v, v2)
        @targetName("plusCVS_Op")
        def +(s: Scalar): ColumnVector = plusCVS(v, s)

    def plusRVM(v: RowVector, m: Matrix): Matrix = plusMRV(m, v)
    def plusRVRV(v1: RowVector, v2: RowVector): RowVector
    def plusRVS(v: RowVector, s: Scalar): RowVector
        
    extension (v: RowVector)
        @targetName("plusRVM_Op")
        def +(m: Matrix): Matrix = plusRVM(v, m)
        @targetName("plusRVRV_Op")
        def +(v2: RowVector): RowVector = plusRVRV(v, v2)
        @targetName("plusRVS_Op")
        def +(s: Scalar): RowVector = plusRVS(v, s)

    def plusSM(s: Scalar, m: Matrix): Matrix = plusMS(m, s)
    def plusSCV(s: Scalar, v: ColumnVector): ColumnVector = plusCVS(v, s)
    def plusSRV(s: Scalar, v: RowVector): RowVector = plusRVS(v, s)
    def plusSS(s1: Scalar, s2: Scalar): Scalar

    extension (s: Scalar)
        @targetName("plusSM_Op")
        def +(m: Matrix): Matrix = plusSM(s, m)
        @targetName("plusSCV_Op")
        def +(v: ColumnVector): ColumnVector = plusSCV(s, v)
        @targetName("plusSRV_Op")
        def +(v: RowVector): RowVector = plusSRV(s, v)
        @targetName("plusSS_Op")
        def +(s2: Scalar): Scalar = plusSS(s, s2)

    def minusMM(m1: Matrix, m2: Matrix): Matrix
    def minusMCV(m: Matrix, v: ColumnVector): Matrix
    def minusMRV(m: Matrix, v: RowVector): Matrix
    def minusMS(m: Matrix, s: Scalar): Matrix

    extension (m1: Matrix)
        @targetName("minusMM_Op")
        def -(m2: Matrix): Matrix = minusMM(m1, m2)
        @targetName("minusMCV_Op")
        def -(v: ColumnVector): Matrix = minusMCV(m1, v)
        @targetName("minusMRV_Op")
        def -(v: RowVector): Matrix = minusMRV(m1, v)
        @targetName("minusMS_Op")
        def -(s: Scalar): Matrix = minusMS(m1, s)

    def minusCVM(v: ColumnVector, m: Matrix): Matrix = minusMCV(m, v)
    def minusCVCV(v1: ColumnVector, v2: ColumnVector): ColumnVector
    def minusCVS(v: ColumnVector, s: Scalar): ColumnVector
    
    extension (v: ColumnVector)
        @targetName("minusCVM_Op")
        def -(m: Matrix): Matrix = minusCVM(v, m)
        @targetName("minusCVCV_Op")
        def -(v2: ColumnVector): ColumnVector = minusCVCV(v, v2)
        @targetName("minusCVS_Op")
        def -(s: Scalar): ColumnVector = minusCVS(v, s)

    def minusRVM(v: RowVector, m: Matrix): Matrix = minusMRV(m, v)
    def minusRVRV(v1: RowVector, v2: RowVector): RowVector
    def minusRVS(v: RowVector, s: Scalar): RowVector

    extension (v: RowVector)
        @targetName("minusRVM_Op")
        def -(m: Matrix): Matrix = minusRVM(v, m)
        @targetName("minusRVRV_Op")
        def -(v2: RowVector): RowVector = minusRVRV(v, v2)
        @targetName("minusRVS_Op")
        def -(s: Scalar): RowVector = minusRVS(v, s)

    def minusSM(s: Scalar, m: Matrix): Matrix = minusMS(m, s)
    def minusSCV(s: Scalar, v: ColumnVector): ColumnVector = minusCVS(v, s)
    def minusSRV(s: Scalar, v: RowVector): RowVector = minusRVS(v, s)
    def minusSS(s1: Scalar, s2: Scalar): Scalar

    extension (s: Scalar)
        @targetName("minusSM_Op")
        def -(m: Matrix): Matrix = minusSM(s, m)
        @targetName("minusSCV_Op")
        def -(v: ColumnVector): ColumnVector = minusSCV(s, v)
        @targetName("minusSRV_Op")
        def -(v: RowVector): RowVector = minusSRV(s, v)
        @targetName("minusSS_Op")
        def -(s2: Scalar): Scalar = minusSS(s, s2)

    def divideMS(m: Matrix, s: Scalar): Matrix
    
    extension (m1: Matrix)
        @targetName("divideMS_Op")
        def /(s: Scalar): Matrix = divideMS(m1, s)

    def divideCVS(v: ColumnVector, s: Scalar): ColumnVector
    
    extension (v: ColumnVector)
        @targetName("divideCVS_Op")
        def /(s: Scalar): ColumnVector = divideCVS(v, s)

    def divideRVS(v: RowVector, s: Scalar): RowVector

    extension (v: RowVector)
        @targetName("divideRVS_Op")
        def /(s: Scalar): RowVector = divideRVS(v, s)

    def divideSM(s: Scalar, m: Matrix): Matrix = divideMS(m, s)
    def divideSCV(s: Scalar, v: ColumnVector): ColumnVector = divideCVS(v, s)
    def divideSRV(s: Scalar, v: RowVector): RowVector = divideRVS(v, s)
    def divideSS(s1: Scalar, s2: Scalar): Scalar

    extension (s: Scalar)
        @targetName("divideSM_Op")
        def /(m: Matrix): Matrix = divideSM(s, m)
        @targetName("divideSCV_Op")
        def /(v: ColumnVector): ColumnVector = divideSCV(s, v)
        @targetName("divideSRV_Op")
        def /(v: RowVector): RowVector = divideSRV(s, v)
        @targetName("divideSS_Op")
        def /(s2: Scalar): Scalar = divideSS(s, s2)

    def foldLeftCV(s: Scalar)(v: ColumnVector)(f: (Scalar, Scalar) => Scalar): Scalar

    extension (v: ColumnVector)
        @targetName("foldLeft_Op")
        def foldLeft(s: Scalar)(f: (Scalar, Scalar) => Scalar): Scalar = foldLeftCV(s)(v)(f)

    def sumCV(v: ColumnVector): Scalar

    extension (v: ColumnVector)
        @targetName("sumCV_Op")
        def sum: Scalar = sumCV(v)

    def sumM(v: Matrix): Scalar

    extension (v: Matrix)
        @targetName("sumM_Op")
        def sum: Scalar = sumM(v)

    def elementWiseTimesMM(m1: Matrix, m2: Matrix): Matrix

    extension (m: Matrix)
        @targetName("elementWiseTimesMM_Op")
        def *:*(m2: Matrix): Matrix = elementWiseTimesMM(m, m2)


    def elementWiseTimesCVCV(v1: ColumnVector, v2: ColumnVector): ColumnVector
        
    extension (v: ColumnVector)
        @targetName("elementWiseTimesCVCV_Op")
        def *:*(v2: ColumnVector): ColumnVector = elementWiseTimesCVCV(v, v2)

    def elementWiseOpsM(m: Matrix, f: Scalar => Scalar): Matrix

    extension (m: Matrix)
        @targetName("elementWiseOpsM_Op")
        def map(f: Scalar => Scalar): Matrix = elementWiseOpsM(m, f)

    def columnWiseOpsM(m: Matrix, f: ColumnVector => ColumnVector): Matrix
    
    extension (m: Matrix)
        @targetName("columnWiseOpsM_Op")
        def mapColumns(f: ColumnVector => ColumnVector): Matrix = columnWiseOpsM(m, f)

    def rowWiseOpsM(m: Matrix, f: RowVector => RowVector): Matrix
    
    extension (m: Matrix)
        @targetName("rowWiseOpsM_Op")
        def mapRows(f: RowVector => RowVector): Matrix = rowWiseOpsM(m, f)

    def elementWiseOpsCV(v: ColumnVector, f: Scalar => Scalar): ColumnVector

    extension (v: ColumnVector)
        @targetName("elementWiseOpsCV_Op")
        def map(f: Scalar => Scalar): ColumnVector = elementWiseOpsCV(v, f)

    def elementWiseOpsRV(v: RowVector, f: Scalar => Scalar): RowVector

    extension (v: RowVector)
        @targetName("elementWiseOpsRV_Op")
        def map(f: Scalar => Scalar): RowVector = elementWiseOpsRV(v, f)

    def applyToScalar(s: Scalar, f: Scalar => Scalar): Scalar = f(s)

    extension (s: Scalar)
        @targetName("applyToScalar_Op")
        def map(f: Scalar => Scalar): Scalar = applyToScalar(s, f)

    extension (s: Scalar)
        @targetName("addSC_Op")
        def +(c: Double): Scalar = 
            s + c.toScalar
        @targetName("subtractSC_Op")
        def -(c: Double): Scalar = 
            s - c.toScalar
        @targetName("multiplySC_Op")
        def *(c: Double): Scalar = 
            s * c.toScalar
        @targetName("divideSC_Op")
        def /(c: Double): Scalar = 
            s / c.toScalar

    extension (cv: ColumnVector)
        @targetName("addCVC_Op")
        def +(c: Double): ColumnVector = 
            cv + c.toScalar
        @targetName("subtractCVC_Op")
        def -(c: Double): ColumnVector = 
            cv - c.toScalar
        @targetName("multiplyCVC_Op")
        def *(c: Double): ColumnVector = 
            cv * c.toScalar
        @targetName("divideCVC_Op")
        def /(c: Double): ColumnVector = 
            cv / c.toScalar

    extension (rv: RowVector)
        @targetName("addRVC_Op")
        def +(c: Double): RowVector = 
            rv + c.toScalar
        @targetName("subtractRVC_Op")
        def -(c: Double): RowVector = 
            rv - c.toScalar
        @targetName("multiplyRVC_Op")
        def *(c: Double): RowVector = 
            rv * c.toScalar
        @targetName("divideRVC_Op")
        def /(c: Double): RowVector = 
            rv / c.toScalar

    extension (m: Matrix)
        @targetName("addMC_Op")
        def +(c: Double): Matrix = 
            m + c.toScalar
        @targetName("subtractMC_Op")
        def -(c: Double): Matrix = 
            m - c.toScalar
        @targetName("multiplyMC_Op")
        def *(c: Double): Matrix = 
            m * c.toScalar
        @targetName("divideMC_Op")
        def /(c: Double): Matrix = 
            m / c.toScalar

    def elementAtM(m: Matrix, rowI: Int, columnJ: Int): Scalar
    def rowAtM(m: Matrix, rowI: Int): RowVector

    def stackRows(rows: RowVector*): Matrix
    def stackRowsSeq(rows: Seq[RowVector]): Matrix = stackRows(rows: _*)

    extension (m: Matrix)
        @targetName("elementAtM_Op")
        def elementAt(rowI: Int, columnJ: Int): Scalar = elementAtM(m, rowI, columnJ)
        @targetName("rowAtM_Op")
        def rowAt(rowI: Int): RowVector = rowAtM(m, rowI)

    def elementsCV(v: ColumnVector): Seq[Scalar] = 
        for (i <- 0 until v.length) yield v.elementAt(i)
    def elementAtCV(v: ColumnVector, i: Int): Scalar

    extension (v: ColumnVector)
        @targetName("elementAtCV_Op")
        def elementAt(i: Int): Scalar = elementAtCV(v, i)
        @targetName("elementsCV_Op")
        def elements: Seq[Scalar] = elementsCV(v)

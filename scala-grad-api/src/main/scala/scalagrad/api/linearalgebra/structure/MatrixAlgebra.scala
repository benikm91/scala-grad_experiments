package scalagrad.api.linearalgebra

import scala.annotation.targetName
import scala.math.Fractional.Implicits._

// No Derivatives Ops
trait LengthOps[ColumnVector, RowVector, Matrix]:
    def numberOfRows(m: Matrix): Int
    def numberOfCols(m: Matrix): Int

    extension (m: Matrix)
        @targetName("numberOfRows_Op")
        def nRows: Int = numberOfRows(m)
        @targetName("numberOfCols_Op")
        def nCols: Int = numberOfCols(m)

    def lengthColumnVector(cv: ColumnVector): Int

    extension (cv: ColumnVector)
        @targetName("lengthColumnVector_Op")
        def length: Int = lengthColumnVector(cv)

    def lengthRowVector(rv: RowVector): Int

    extension (rv: RowVector)
        @targetName("lengthRowVector_Op")
        def length: Int = lengthRowVector(rv)

// Maybe support basic operations with Double, Float, Int etc. types?
trait LiftOps[Scalar]:
    def liftToScalar(d: Int): Scalar
    def liftToScalar(d: Double): Scalar

trait MatrixOps[Matrix, Scalar]:

    def trace(m: Matrix): Scalar 

    def inverse(m: Matrix): Matrix

    extension (m: Matrix)
        @targetName("inverse_Op")
        def inv: Matrix = inverse(m)

    def determinant(m: Matrix): Scalar

    extension (m: Matrix)
        @targetName("determinant_Op")
        def det: Scalar = determinant(m)

// Derivatives Ops
trait TransposeOps[ColumnVector, RowVector, Matrix]:

    def transpose(m: Matrix): Matrix

    extension (m: Matrix)
        @targetName("transpose_Op")
        def t: Matrix = transpose(m)

    def transposeColumVector(cv: ColumnVector): RowVector

    extension (cv: ColumnVector)
        @targetName("transposeColumnVector_Op")
        def t: RowVector = transposeColumVector(cv)

    def transposeRowVector(v: RowVector): ColumnVector
    
    extension (rv: RowVector)
        @targetName("transposeRowVector_Op")
        def t: ColumnVector = transposeRowVector(rv)

// Derivatives Ops
trait NegateOps[Scalar, ColumnVector, RowVector, Matrix]:
 
    def negateS(s: Scalar): Scalar

    extension (s: Scalar)
        @targetName("negate_Op")
        def unary_- : Scalar = negateS(s)

    def negateCV(cv: ColumnVector): ColumnVector

    extension (cv: ColumnVector)
        @targetName("negateCV_Op")
        def unary_- : ColumnVector = negateCV(cv)

    def negateRV(rv: RowVector): RowVector
    
    extension (rv: RowVector)
        @targetName("negateRV_Op")
        def unary_- : RowVector = negateRV(rv)
    
    def negateM(m: Matrix): Matrix

    extension (m: Matrix)
        @targetName("negateM_Op")
        def unary_- : Matrix = negateM(m)

// Derivatives Ops
trait ScalarInvertOps[Scalar]:
    def invert(s: Scalar): Scalar

    extension (s: Scalar)
        @targetName("invert_Op")
        def unary_~ : Scalar = invert(s)

// Derivatives Ops
trait BasicOps[Scalar, ColumnVector, RowVector, Matrix]:

    this: NegateOps[Scalar, ColumnVector, RowVector, Matrix] 
        with ScalarInvertOps[Scalar] =>

    def timesMM(m1: Matrix, m2: Matrix): Matrix
    def timesMCV(m: Matrix, cv: ColumnVector): ColumnVector
    def timesMS(m: Matrix, s: Scalar): Matrix
    
    extension (m: Matrix)
        @targetName("timesMM_Op")
        def *(m2: Matrix): Matrix = timesMM(m, m2)
        @targetName("timesMCV_Op")
        def *(cv: ColumnVector): ColumnVector = timesMCV(m, cv)
        @targetName("timesMS_Op")
        def *(s: Scalar): Matrix = timesMS(m, s)
        
    def timesCVRV(cv: ColumnVector, rv: RowVector): Matrix
    def timesCVS(cvv: ColumnVector, s: Scalar): ColumnVector

    extension (cv: ColumnVector)
        @targetName("timesCVRV_Op")
        def *(rv: RowVector): Matrix = timesCVRV(cv, rv)
        @targetName("timesCVS_Op")
        def *(s: Scalar): ColumnVector = timesCVS(cv, s)

    def timesRVM(rv: RowVector, m: Matrix): RowVector
    def timesRVCV(rv: RowVector, cv: ColumnVector): Scalar
    def timesRVS(rv: RowVector, s: Scalar): RowVector

    extension (rv: RowVector)
        @targetName("timesRVM_Op")
        def *(m: Matrix): RowVector = timesRVM(rv, m)
        @targetName("timesRVCV_Op")
        def *(cv: ColumnVector): Scalar = timesRVCV(rv, cv)
        @targetName("timesRVS_Op")
        def *(s: Scalar): RowVector = timesRVS(rv, s)

    def timesSM(s: Scalar, m: Matrix): Matrix = timesMS(m, s)
    def timesSCV(s: Scalar, cv: ColumnVector): ColumnVector = timesCVS(cv, s)
    def timesSRV(s: Scalar, rv: RowVector): RowVector = timesRVS(rv, s)
    def timesSS(s1: Scalar, s2: Scalar): Scalar

    extension (s: Scalar)
        @targetName("timesSM_Op")
        def *(m: Matrix): Matrix = timesSM(s, m)
        @targetName("timesSCV_Op")
        def *(cv: ColumnVector): ColumnVector = timesSCV(s, cv)
        @targetName("timesSRV_Op")
        def *(rv: RowVector): RowVector = timesSRV(s, rv)
        @targetName("timesSS_Op")
        def *(s2: Scalar): Scalar = timesSS(s, s2)

    def plusMM(m1: Matrix, m2: Matrix): Matrix
    def plusMCV(m: Matrix, cv: ColumnVector): Matrix
    def plusMRV(m: Matrix, rv: RowVector): Matrix
    def plusMS(m: Matrix, s: Scalar): Matrix

    extension (m: Matrix)
        @targetName("plusMM_Op")
        def +(m2: Matrix): Matrix = plusMM(m, m2)
        @targetName("plusMCV_Op")
        def +(cv: ColumnVector): Matrix = plusMCV(m, cv)
        @targetName("plusMRV_Op")
        def +(rv: RowVector): Matrix = plusMRV(m, rv)
        @targetName("plusMS_Op")
        def +(s: Scalar): Matrix = plusMS(m, s)
        
    def plusCVM(cv: ColumnVector, m: Matrix): Matrix = plusMCV(m, cv)
    def plusCVCV(cv1: ColumnVector, cv2: ColumnVector): ColumnVector
    def plusCVS(cv: ColumnVector, s: Scalar): ColumnVector

    extension (cv: ColumnVector)
        @targetName("plusCVM_Op")
        def +(m: Matrix): Matrix = plusCVM(cv, m)
        @targetName("plusCVCV_Op")
        def +(cv2: ColumnVector): ColumnVector = plusCVCV(cv, cv2)
        @targetName("plusCVS_Op")
        def +(s: Scalar): ColumnVector = plusCVS(cv, s)

    def plusRVM(rv: RowVector, m: Matrix): Matrix = plusMRV(m, rv)
    def plusRVRV(rv1: RowVector, rv2: RowVector): RowVector
    def plusRVS(rv: RowVector, s: Scalar): RowVector
        
    extension (rv: RowVector)
        @targetName("plusRVM_Op")
        def +(m: Matrix): Matrix = plusRVM(rv, m)
        @targetName("plusRVRV_Op")
        def +(rv2: RowVector): RowVector = plusRVRV(rv, rv2)
        @targetName("plusRVS_Op")
        def +(s: Scalar): RowVector = plusRVS(rv, s)

    def plusSM(s: Scalar, m: Matrix): Matrix = plusMS(m, s)
    def plusSCV(s: Scalar, cv: ColumnVector): ColumnVector = plusCVS(cv, s)
    def plusSRV(s: Scalar, rv: RowVector): RowVector = plusRVS(rv, s)
    def plusSS(s1: Scalar, s2: Scalar): Scalar

    extension (s: Scalar)
        @targetName("plusSM_Op")
        def +(m: Matrix): Matrix = plusSM(s, m)
        @targetName("plusSCV_Op")
        def +(cv: ColumnVector): ColumnVector = plusSCV(s, cv)
        @targetName("plusSRV_Op")
        def +(rv: RowVector): RowVector = plusSRV(s, rv)
        @targetName("plusSS_Op")
        def +(s2: Scalar): Scalar = plusSS(s, s2)

    def minusMM(m1: Matrix, m2: Matrix): Matrix = plusMM(m1, negateM(m2))
    def minusMCV(m: Matrix, cv: ColumnVector): Matrix = plusMCV(m, negateCV(cv))
    def minusMRV(m: Matrix, rv: RowVector): Matrix = plusMRV(m, negateRV(rv))
    def minusMS(m: Matrix, s: Scalar): Matrix = plusMS(m, negateS(s))

    extension (m1: Matrix)
        @targetName("minusMM_Op")
        def -(m2: Matrix): Matrix = minusMM(m1, m2)
        @targetName("minusMCV_Op")
        def -(cv: ColumnVector): Matrix = minusMCV(m1, cv)
        @targetName("minusMRV_Op")
        def -(rv: RowVector): Matrix = minusMRV(m1, rv)
        @targetName("minusMS_Op")
        def -(s: Scalar): Matrix = minusMS(m1, s)

    def minusCVM(cv: ColumnVector, m: Matrix): Matrix = minusMCV(m, cv)
    def minusCVCV(cv1: ColumnVector, cv2: ColumnVector): ColumnVector = plusCVCV(cv1, negateCV(cv2))
    def minusCVS(cv: ColumnVector, s: Scalar): ColumnVector = plusCVS(cv, negateS(s))
    
    extension (cv: ColumnVector)
        @targetName("minusCVM_Op")
        def -(m: Matrix): Matrix = minusCVM(cv, m)
        @targetName("minusCVCV_Op")
        def -(cv2: ColumnVector): ColumnVector = minusCVCV(cv, cv2)
        @targetName("minusCVS_Op")
        def -(s: Scalar): ColumnVector = minusCVS(cv, s)

    def minusRVM(rv: RowVector, m: Matrix): Matrix = minusMRV(m, rv)
    def minusRVRV(rv1: RowVector, rv2: RowVector): RowVector = plusRVRV(rv1, negateRV(rv2))
    def minusRVS(rv: RowVector, s: Scalar): RowVector = plusRVS(rv, negateS(s))

    extension (rv: RowVector)
        @targetName("minusRVM_Op")
        def -(m: Matrix): Matrix = minusRVM(rv, m)
        @targetName("minusRVRV_Op")
        def -(rv2: RowVector): RowVector = minusRVRV(rv, rv2)
        @targetName("minusRVS_Op")
        def -(s: Scalar): RowVector = minusRVS(rv, s)

    def minusSM(s: Scalar, m: Matrix): Matrix = minusMS(m, s)
    def minusSCV(s: Scalar, cv: ColumnVector): ColumnVector = minusCVS(cv, s)
    def minusSRV(s: Scalar, rv: RowVector): RowVector = minusRVS(rv, s)
    def minusSS(s1: Scalar, s2: Scalar): Scalar = plusSS(s1, negateS(s2))

    extension (s: Scalar)
        @targetName("minusSM_Op")
        def -(m: Matrix): Matrix = minusSM(s, m)
        @targetName("minusSCV_Op")
        def -(cv: ColumnVector): ColumnVector = minusSCV(s, cv)
        @targetName("minusSRV_Op")
        def -(rv: RowVector): RowVector = minusSRV(s, rv)
        @targetName("minusSS_Op")
        def -(s2: Scalar): Scalar = minusSS(s, s2)

    def divideMS(m: Matrix, s: Scalar): Matrix = timesMS(m, invert(s))
    
    extension (m: Matrix)
        @targetName("divideMS_Op")
        def /(s: Scalar): Matrix = divideMS(m, s)

    def divideCVS(cv: ColumnVector, s: Scalar): ColumnVector = timesCVS(cv, invert(s))
    
    extension (v: ColumnVector)
        @targetName("divideCVS_Op")
        def /(s: Scalar): ColumnVector = divideCVS(v, s)

    def divideRVS(v: RowVector, s: Scalar): RowVector = timesRVS(v, invert(s))

    extension (v: RowVector)
        @targetName("divideRVS_Op")
        def /(s: Scalar): RowVector = divideRVS(v, s)

    def divideSM(s: Scalar, m: Matrix): Matrix = divideMS(m, s)
    def divideSCV(s: Scalar, v: ColumnVector): ColumnVector = divideCVS(v, s)
    def divideSRV(s: Scalar, v: RowVector): RowVector = divideRVS(v, s)
    def divideSS(s1: Scalar, s2: Scalar): Scalar = timesSS(s1, invert(s2))

    extension (s: Scalar)
        @targetName("divideSM_Op")
        def /(m: Matrix): Matrix = divideSM(s, m)
        @targetName("divideSCV_Op")
        def /(v: ColumnVector): ColumnVector = divideSCV(s, v)
        @targetName("divideSRV_Op")
        def /(v: RowVector): RowVector = divideSRV(s, v)
        @targetName("divideSS_Op")
        def /(s2: Scalar): Scalar = divideSS(s, s2)

    
    def elementWiseMultiplyMM(m1: Matrix, m2: Matrix): Matrix

    extension (m: Matrix)
        @targetName("elementWiseMultiplyMM_Op")
        def *:*(m2: Matrix): Matrix = elementWiseMultiplyMM(m, m2)

    def elementWiseMultiplyCVCV(cv1: ColumnVector, cv2: ColumnVector): ColumnVector
        
    extension (cv: ColumnVector)
        @targetName("elementWiseMultiplyCVCV_Op")
        def *:*(cv2: ColumnVector): ColumnVector = elementWiseMultiplyCVCV(cv, cv2)
    
    def elementWiseMultiplyRVRV(rv1: RowVector, rv2: RowVector): RowVector

    extension (rv: RowVector)
        @targetName("elementWiseMultiplyRVRV_Op")
        def *:*(rv2: RowVector): RowVector = elementWiseMultiplyRVRV(rv, rv2)


// Could be implemented with foldLeft, maybe allow optimized implementations for reduction
trait SumOps[Scalar, ColumnVector, RowVector, Matrix]:
    def sumCV(cv: ColumnVector): Scalar
    def sumRV(rv: RowVector): Scalar
    def sumM(m: Matrix): Scalar

    extension (cv: ColumnVector)
        @targetName("sumCV_Op")
        def sum: Scalar = sumCV(cv)

    extension (rv: RowVector)
        @targetName("sumRV_Op")
        def sum: Scalar = sumRV(rv)

    extension (m: Matrix)
        @targetName("sumM_Op")
        def sum: Scalar = sumM(m)


// No Derivative Ops
trait FoldLeftOps[Scalar, ColumnVector, RowVector, Matrix]:

    this: LengthOps[ColumnVector, RowVector, Matrix]
        with AccessOps[Scalar, ColumnVector, RowVector, Matrix] =>

    def foldLeftM(m: Matrix)(s: Scalar)(f: (Scalar, Scalar) => Scalar): Scalar = 
        m.elements.foldLeft(s)(f)

    def foldLeftCV(cv: ColumnVector)(s: Scalar)(f: (Scalar, Scalar) => Scalar): Scalar = 
        cv.elements.foldLeft(s)(f)

    def foldLeftRV(rv: RowVector)(s: Scalar)(f: (Scalar, Scalar) => Scalar): Scalar =
        rv.elements.foldLeft(s)(f)

    extension (m: Matrix)
        @targetName("foldLeftM_Op")
        def foldLeft(s: Scalar)(f: (Scalar, Scalar) => Scalar): Scalar = foldLeftM(m)(s)(f)

    extension (cv: ColumnVector)
        @targetName("foldLeftCV_Op")
        def foldLeft(s: Scalar)(f: (Scalar, Scalar) => Scalar): Scalar = foldLeftCV(cv)(s)(f)

    extension (rv: RowVector)
        @targetName("foldLeftRV_Op")
        def foldLeft(s: Scalar)(f: (Scalar, Scalar) => Scalar): Scalar = foldLeftRV(rv)(s)(f)

trait AccessOps[Scalar, ColumnVector, RowVector, Matrix]:

    this: LengthOps[ColumnVector, RowVector, Matrix] =>

    def elementsM(m: Matrix): Seq[Scalar] = 
        for {
            jCol <- 0 until m.nCols
            iRow <- 0 until m.nRows
        } yield m.elementAt(iRow, jCol)

    def rowsM(m: Matrix): Seq[RowVector] =
        for (iRow <- 0 until m.nRows) yield m.rowAt(iRow)

    def columns(m: Matrix): Seq[ColumnVector] =
        for (jCol <- 0 until m.nCols) yield m.columnAt(jCol)

    def elementsCV(cv: ColumnVector): Seq[Scalar] =
        for (i <- 0 until cv.length) yield cv.elementAt(i)

    def elementsRV(rv: RowVector): Seq[Scalar] = 
        for (i <- 0 until rv.length) yield rv.elementAt(i)

    def elementAtM(m: Matrix, iRow: Int, jColumn: Int): Scalar
    def rowAtM(m: Matrix, iRow: Int): RowVector
    def columnAtM(m: Matrix, jColumn: Int): ColumnVector

    def elementAtCV(cv: ColumnVector, iRow: Int): Scalar
    def elementAtRV(rv: RowVector, jColumn: Int): Scalar

    extension (m: Matrix)
        @targetName("elementsM_Op")
        def elements: Seq[Scalar] = elementsM(m)
        @targetName("elementAtM_Op")
        def elementAt(iRow: Int, jColumn: Int): Scalar = elementAtM(m, iRow, jColumn)
        @targetName("rowAtM_Op")
        def rowAt(iRow: Int): RowVector = rowAtM(m, iRow)
        @targetName("columnAtM_Op")
        def columnAt(jColumn: Int): ColumnVector = columnAtM(m, jColumn)

    extension (cv: ColumnVector)
        @targetName("elementsCV_Op")
        def elements: Seq[Scalar] = elementsCV(cv)
        @targetName("elementAtCV_Op")
        def elementAt(iRow: Int): Scalar = elementAtCV(cv, iRow)

    extension (rv: RowVector)
        @targetName("elementsRV_Op")
        def elements: Seq[Scalar] = elementsRV(rv)
        @targetName("elementAtRV_Op")
        def elementAt(jColumn: Int): Scalar = elementAtRV(rv, jColumn)


// Derivative Ops
trait CreateOps[Scalar, ColumnVector, RowVector, Matrix]:

    def createMatrix(nRows: Int, nCols: Int, elements: Seq[Scalar]): Matrix
    def stackRows(rows: Seq[RowVector]): Matrix
    def stackColumns(columns: Seq[ColumnVector]): Matrix
    def createColumnVector(nRows: Int, nCols: Int, elements: Seq[Scalar]): ColumnVector
    def createRowVector(nRows: Int, nCols: Int, elements: Seq[Scalar]): RowVector


// No Derivative Ops
trait ElementWiseOps[Scalar, ColumnVector, RowVector, Matrix]:

    this: AccessOps[Scalar, ColumnVector, RowVector, Matrix] 
        with LengthOps[ColumnVector, RowVector, Matrix]
        with TransposeOps[ColumnVector, RowVector, Matrix]
        with CreateOps[Scalar, ColumnVector, RowVector, Matrix] =>

    def elementWiseOpM(m: Matrix, op: Scalar => Scalar): Matrix = 
        createMatrix(m.nRows, m.nCols, m.elements.map(op))

    def columnWiseOpsM(m: Matrix, op: ColumnVector => ColumnVector): Matrix
    def rowWiseOpsM(m: Matrix, op: RowVector => RowVector): Matrix

    def elementWiseOpCV(cv: ColumnVector, op: Scalar => Scalar): ColumnVector = 
        createColumnVector(cv.length, 1, cv.elements.map(op))
    
    def elementWiseOpRV(rv: RowVector, op: Scalar => Scalar): RowVector = 
        createRowVector(1, rv.length, rv.elements.map(op))

    extension (m: Matrix)
        @targetName("elementWiseOpM_Op")
        def mapElements(op: Scalar => Scalar): Matrix = elementWiseOpM(m, op)
        @targetName("columnWiseOpsM_Op")
        def mapColumns(op: ColumnVector => ColumnVector): Matrix = columnWiseOpsM(m, op)
        @targetName("rowWiseOpsM_Op")
        def mapRows(op: RowVector => RowVector): Matrix = rowWiseOpsM(m, op)

    extension (cv: ColumnVector)
        @targetName("elementWiseOpCV_Op")
        def mapElements(op: Scalar => Scalar): ColumnVector = elementWiseOpCV(cv, op)

    extension (rv: RowVector)
        @targetName("elementWiseOpRV_Op")
        def mapElements(op: Scalar => Scalar): RowVector = elementWiseOpRV(rv, op)

trait MatrixAlgebra[Scalar, ColumnVector, RowVector, Matrix] 
    extends LengthOps[ColumnVector, RowVector, Matrix]
    with LiftOps[Scalar]
    with MatrixOps[Matrix, Scalar]
    with TransposeOps[ColumnVector, RowVector, Matrix]
    with NegateOps[Scalar, ColumnVector, RowVector, Matrix]
    with ScalarInvertOps[Scalar]
    with BasicOps[Scalar, ColumnVector, RowVector, Matrix]
    with SumOps[Scalar, ColumnVector, RowVector, Matrix]
    with FoldLeftOps[Scalar, ColumnVector, RowVector, Matrix]
    with AccessOps[Scalar, ColumnVector, RowVector, Matrix]
    with CreateOps[Scalar, ColumnVector, RowVector, Matrix]
    with ElementWiseOps[Scalar, ColumnVector, RowVector, Matrix]
:

    override def trace(m: Matrix): Scalar = 
        val n = m.nRows min m.nCols
        var res = liftToScalar(0)
        for i <- 0 until n do
            res = res + m.elementAt(i, i)
        res
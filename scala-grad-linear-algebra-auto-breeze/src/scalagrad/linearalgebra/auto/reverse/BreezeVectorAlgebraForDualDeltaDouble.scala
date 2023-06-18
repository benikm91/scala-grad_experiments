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
import DualDeltaScalar.{deltaLet => deltaLetScalar}
import scalagrad.linearalgebra.auto.forward.dual.DualNumberScalar
import scalagrad.linearalgebra.api.dual.DualScalar

object BreezeVectorAlgebraForDualDeltaDouble extends BreezeVectorAlgebraForDualDouble:

    override type ScalarD = DeltaMonad[Double, DeltaScalar[Double]]
    override type ColumnVectorD = DeltaMonad[Double, DeltaColumnVector[Double]]

    override type RowVectorD = DeltaMonad[Double, DeltaRowVector[Double]]

    override type MatrixD = DeltaMonad[Double, DeltaMatrix[Double]]

    override type Scalar = DualDeltaScalar[Double]
    override type ColumnVector = DualDeltaColumnVector[Double]
    override type RowVector = DualDeltaRowVector[Double]
    override type Matrix = DualDeltaMatrix[Double]


    def createScalar(value: Double, dual: ScalarD): Scalar = DualDeltaScalar(value, dual)

    def createColumnVector(value: DenseVector[Double], dual: ColumnVectorD): ColumnVector = DualDeltaColumnVector(value, dual)
 
    def createRowVector(value: Transpose[DenseVector[Double]], dual: RowVectorD): RowVector = DualDeltaRowVector(value, dual)

    def createMatrix(value: DenseMatrix[Double], dual: MatrixD): Matrix = DualDeltaMatrix(value, dual)
    
    override def zeroD: ScalarD = 
        DeltaMonad[Double, DeltaScalar[Double]](state => (state, DeltaScalar.Zero(0.0)))
    
    def deltaLetColumnVector(delta: DeltaColumnVector[Double]): DeltaMonad[Double, DeltaId] = DeltaMonad[Double, DeltaId](next => 
        next.getColumnVector(delta) match
            case None => next.addColumnVector(delta)
            case Some(value) => (next, value)
    )

    def deltaLetRowVector(delta: DeltaRowVector[Double]): DeltaMonad[Double, DeltaId] = DeltaMonad[Double, DeltaId](next => 
        next.getRowVector(delta) match
            case None => next.addRowVector(delta)
            case Some(value) => (next, value)
    )

    def deltaLetMatrix(delta: DeltaMatrix[Double]): DeltaMonad[Double, DeltaId] = DeltaMonad[Double, DeltaId](next => 
        next.getMatrix(delta) match
            case None => next.addMatrix(delta)
            case Some(value) => (next, value)
    )

    def dTranspose(m: MatrixD): MatrixD = 
        for {
            dm <- m
            newId <- deltaLetMatrix(DeltaMatrix.Transpose(dm))
        } yield DeltaMatrix.Val(newId)

    def dTransposeColumVector(v: ColumnVectorD): RowVectorD = 
        for {
            dv <- v
            newId <- deltaLetRowVector(DeltaRowVector.Transpose(dv))
        } yield DeltaRowVector.Val(newId)

    def dTransposeRowVector(v: RowVectorD): ColumnVectorD = 
        for {
            dv <- v
            newId <- deltaLetColumnVector(DeltaColumnVector.Transpose(dv))
        } yield DeltaColumnVector.Val(newId)

    def timesDMM(dm: MatrixD, m: DenseMatrix[Double]): MatrixD = 
        for {
            dm <- dm
            newId <- deltaLetMatrix(DeltaMatrix.MatrixDotDMM(dm, m))
        } yield DeltaMatrix.Val(newId)

    def timesMDM(m: DenseMatrix[Double], dm: MatrixD): MatrixD = 
        for {
            dm <- dm
            newId <- deltaLetMatrix(DeltaMatrix.MatrixDotMDM(m, dm))
        } yield DeltaMatrix.Val(newId)

    def addDMDM(dm1: MatrixD, dm2: MatrixD): MatrixD = 
        for {
            dm1 <- dm1
            dm2 <- dm2
            newId <- deltaLetMatrix(DeltaMatrix.AddDMDM(dm1, dm2))
        } yield DeltaMatrix.Val(newId)    

    def timesDMCV(dm: MatrixD, v: DenseVector[Double]): ColumnVectorD = 
        for {
            dv <- dm
            newId <- deltaLetColumnVector(DeltaColumnVector.MatrixDotDMCV(dv, v))
        } yield DeltaColumnVector.Val(newId)

    def timesMDCV(m: DenseMatrix[Double], dv: ColumnVectorD): ColumnVectorD = 
        for {
            dv <- dv
            newId <- deltaLetColumnVector(DeltaColumnVector.MatrixDotMDCV(m, dv))
        } yield DeltaColumnVector.Val(newId)
        
    def addDCVDCV(dv1: ColumnVectorD, dv2: ColumnVectorD): ColumnVectorD = 
        for {
            dv1 <- dv1
            dv2 <- dv2
            newId <- deltaLetColumnVector(DeltaColumnVector.AddDCVDCV(dv1, dv2))
        } yield DeltaColumnVector.Val(newId)


    def timesDMS(dm: MatrixD, s: Double): MatrixD = 
        for {
            dm <- dm
            newId <- deltaLetMatrix(DeltaMatrix.MatrixDotDMS(dm, s))
        } yield DeltaMatrix.Val(newId)

    def timesMDS(m: DenseMatrix[Double], ds: ScalarD): MatrixD = 
        for {
            ds <- ds
            newId <- deltaLetMatrix(DeltaMatrix.MatrixDotMDS(m, ds))
        } yield DeltaMatrix.Val(newId)
    
    def timesDCVRV(dv: ColumnVectorD, v: Transpose[DenseVector[Double]]): MatrixD =
        for {
            dv <- dv
            newId <- deltaLetMatrix(DeltaMatrix.MatrixDotDCVRV(dv, v))
        } yield DeltaMatrix.Val(newId)

    def timesCVDRV(v: DenseVector[Double], dv: RowVectorD): MatrixD = 
        for {
            dv <- dv
            newId <- deltaLetMatrix(DeltaMatrix.MatrixDotCVDRV(v, dv))
        } yield DeltaMatrix.Val(newId)

    def timesDCVS(dv: ColumnVectorD, s: Double): ColumnVectorD = 
        for {
            dv <- dv
            newId <- deltaLetColumnVector(DeltaColumnVector.MatrixDotDCVS(dv, s))
        } yield DeltaColumnVector.Val(newId)

    def timesCVDS(v: DenseVector[Double], ds: ScalarD): ColumnVectorD = 
        for {
            ds <- ds
            newId <- deltaLetColumnVector(DeltaColumnVector.MatrixDotCVDS(v, ds))
        } yield DeltaColumnVector.Val(newId)

    def addDRVDRV(dv1: RowVectorD, dv2: RowVectorD): RowVectorD = 
        for { 
            dv1 <- dv1
            dv2 <- dv2
            newId <- deltaLetRowVector(DeltaRowVector.AddVV(dv1, dv2))
        } yield DeltaRowVector.Val(newId)

    def timesDRVM(drv: RowVectorD, m: DenseMatrix[Double]): RowVectorD = 
        for {
            drv <- drv
            newId <- deltaLetRowVector(DeltaRowVector.MatrixDotDRVM(drv, m))
        } yield DeltaRowVector.Val(newId)

    def timesRVDM(rv: Transpose[DenseVector[Double]], dm: MatrixD): RowVectorD = 
        for {
            dm <- dm
            newId <- deltaLetRowVector(DeltaRowVector.MatrixDotRVDM(rv, dm))
        } yield DeltaRowVector.Val(newId)    

    def addDSDS(ds1: ScalarD, ds2: ScalarD): ScalarD = 
        for {
            ds1 <- ds1
            ds2 <- ds2
            newId <- deltaLetScalar(DeltaScalar.Add(ds1, ds2))
        } yield DeltaScalar.Val(newId)
    
    def timesDRVCV(dv: RowVectorD, v: DenseVector[Double]): ScalarD = 
        for {
            dv <- dv
            newId <- deltaLetScalar(DeltaScalar.MatrixDotDRVCV(dv, v))
        } yield DeltaScalar.Val(newId)
        
    def timesRVDCV(v: Transpose[DenseVector[Double]], dv: ColumnVectorD): ScalarD = 
        for {
            dv <- dv
            newId <- deltaLetScalar(DeltaScalar.MultiplyRVDCV(v, dv))
        } yield DeltaScalar.Val(newId)

    def timesDRVS(dv: RowVectorD, s: Double): RowVectorD =  
        for {
            dv <- dv
            newId <- deltaLetRowVector(DeltaRowVector.Scale(dv, s))
        } yield DeltaRowVector.Val(newId)

    def timesRVDS(v: Transpose[DenseVector[Double]], ds: ScalarD): RowVectorD =
        for {
            ds <- ds
            newId <- deltaLetRowVector(DeltaRowVector.MatrixDotRVDS(v, ds))
        } yield DeltaRowVector.Val(newId)
    def timesSDS(s: Double, ds: ScalarD): ScalarD = 
        for {
            ds <- ds
            newId <- deltaLetScalar(DeltaScalar.Scale(ds, s))
        } yield DeltaScalar.Val(newId)

    def addDMDCV(dm: MatrixD, dv: ColumnVectorD): MatrixD = 
        for {
            dm <- dm
            dv <- dv
            newId <- deltaLetMatrix(DeltaMatrix.AddDMDCV(dm, dv))
        } yield DeltaMatrix.Val(newId)

    def addDMDRV(dm: MatrixD, dv: RowVectorD): MatrixD = for {
            dm <- dm
            dv <- dv
            newId <- deltaLetMatrix(DeltaMatrix.AddDMDRV(dm, dv))
        } yield DeltaMatrix.Val(newId)
    def addDMDS(dm: MatrixD, ds: ScalarD): MatrixD = 
        for {
            dm <- dm
            ds <- ds
            newId <- deltaLetMatrix(DeltaMatrix.AddDMDS(dm, ds))
        } yield DeltaMatrix.Val(newId)

    def addDCVDS(dv: ColumnVectorD, ds: ScalarD): ColumnVectorD = 
        for {
            dv <- dv
            ds <- ds
            newId <- deltaLetColumnVector(DeltaColumnVector.AddVS(dv, ds))
        } yield DeltaColumnVector.Val(newId)

    def addDRVDS(dv: RowVectorD, ds: ScalarD): RowVectorD =
        for {
            dv <- dv
            ds <- ds
            newId <- deltaLetRowVector(DeltaRowVector.AddDRVDS(dv, ds))
        } yield DeltaRowVector.Val(newId)

    def subDMDM(dm1: MatrixD, dm2: MatrixD): MatrixD = 
        for {
            dm1 <- dm1
            dm2 <- dm2
            newId <- deltaLetMatrix(DeltaMatrix.MinusDMDM(dm1, dm2))
        } yield DeltaMatrix.Val(newId)
        
    def subDMDCV(dm: MatrixD, dcv: ColumnVectorD): MatrixD = 
        for {
            dm <- dm
            dcv <- dcv
            newId <- deltaLetMatrix(DeltaMatrix.MinusDMDCV(dm, dcv))
        } yield DeltaMatrix.Val(newId)

    def subDMDRV(dm: MatrixD, drv: RowVectorD): MatrixD = 
        for {
            dm <- dm
            drv <- drv
            newId <- deltaLetMatrix(DeltaMatrix.MinusDMDRV(dm, drv))
        } yield DeltaMatrix.Val(newId)    

    def subDMDS(dm: MatrixD, ds: ScalarD): MatrixD = 
        for {
            dm <- dm
            ds <- ds
            newId <- deltaLetMatrix(DeltaMatrix.MinusDMDS(dm, ds))
        } yield DeltaMatrix.Val(newId)    


    def subDCVDCV(dv1: ColumnVectorD, dv2: ColumnVectorD): ColumnVectorD = 
        for {
            dv1 <- dv1
            dv2 <- dv2
            newId <- deltaLetColumnVector(DeltaColumnVector.MinusVV(dv1, dv2))
        } yield DeltaColumnVector.Val(newId)

    def subDCVDS(dv: ColumnVectorD, ds: ScalarD): ColumnVectorD = 
        for {
            dv <- dv
            ds <- ds
            newId <- deltaLetColumnVector(DeltaColumnVector.MinusVS(dv, ds))
        } yield DeltaColumnVector.Val(newId)
        
    def subDRVDRV(dv1: RowVectorD, dv2: RowVectorD): RowVectorD =
        for {
            dv1 <- dv1
            dv2 <- dv2
            newId <- deltaLetRowVector(DeltaRowVector.MinusDRVDRV(dv1, dv2))
        } yield DeltaRowVector.Val(newId)
    def subDRVDS(dv: RowVectorD, ds: ScalarD): RowVectorD = 
        for {
            dv <- dv
            ds <- ds
            newId <- deltaLetRowVector(DeltaRowVector.MinusDRVDS(dv, ds))
        } yield DeltaRowVector.Val(newId)    

    def subDSDS(ds1: ScalarD, ds2: ScalarD): ScalarD = 
        for {
            ds1 <- ds1
            ds2 <- ds2
            newId <- deltaLetScalar(DeltaScalar.Sub(ds1, ds2))
        } yield DeltaScalar.Val(newId)

    def divideDMS(dm: MatrixD, s: Double): MatrixD = 
        for {
            dm <- dm
            newId <- deltaLetMatrix(DeltaMatrix.Div(dm, s))
        } yield DeltaMatrix.Val(newId)

    def divideDCVS(dv: ColumnVectorD, s: Double): ColumnVectorD = 
        for {
            dv <- dv
            newId <- deltaLetColumnVector(DeltaColumnVector.Div(dv, s))
        } yield DeltaColumnVector.Val(newId)
    def divideDRVS(dv: RowVectorD, s: Double): RowVectorD =
        for {
            dv <- dv
            newId <- deltaLetRowVector(DeltaRowVector.Div(dv, s))
        } yield DeltaRowVector.Val(newId)
    def divideDSS(ds: ScalarD, s: Double): ScalarD = 
        for {
            ds <- ds
            newId <- deltaLetScalar(DeltaScalar.Div(ds, s))
        } yield DeltaScalar.Val(newId)

    def sumDCV(dv: ColumnVectorD, vLength: Int): ScalarD = 
        for {
            dv <- dv
            newId <- deltaLetScalar(DeltaScalar.Sum(dv, vLength))
        } yield DeltaScalar.Val(newId)

    override def sumDM(dm: MatrixD, nRows: Int, nCols: Int): ScalarD = 
        for {
            dm <- dm
            newId <- deltaLetScalar(DeltaScalar.SumM(dm, nRows, nCols))
        } yield DeltaScalar.Val(newId)

    def timesElementWiseMDM(m1: DenseMatrix[Double], dm2: MatrixD): MatrixD = 
        for {
            dm2 <- dm2
            newId <- deltaLetMatrix(DeltaMatrix.ElementWiseScale(m1, dm2))
        } yield DeltaMatrix.Val(newId)

    def timesElementWiseCVDCV(v1: DenseVector[Double], dv2: ColumnVectorD): ColumnVectorD = 
        for {
            dv2 <- dv2
            newId <- deltaLetColumnVector(DeltaColumnVector.ElementWiseScale(v1, dv2))
        } yield DeltaColumnVector.Val(newId)

    def timesElementWiseRVDRV(v1: Transpose[DenseVector[Double]], dv2: RowVectorD): RowVectorD = 
        for {
            dv2 <- dv2
            newId <- deltaLetRowVector(DeltaRowVector.ElementWiseScale(v1, dv2))
        } yield DeltaRowVector.Val(newId)

    override def foldLeftCV(s: Scalar)(v: ColumnVector)(f: (Scalar, Scalar) => Scalar): Scalar =
        var current = s
        for(i <- 0 until v.length)
            f(current, v.elementAt(i))
        current

    override def elementWiseTimesMDM(m: DenseMatrix[Double], dm: MatrixD): MatrixD = 
        for {
            dm <- dm
            newId <- deltaLetMatrix(DeltaMatrix.ElementWiseScale(m, dm))
        } yield DeltaMatrix.Val(newId)

    override def elementWiseTimesCVDCV(v: DenseVector[Double], dv: ColumnVectorD): ColumnVectorD = 
        for {
            dv <- dv
            newId <- deltaLetColumnVector(DeltaColumnVector.ElementWiseScale(v, dv))
        } yield DeltaColumnVector.Val(newId)

    def elementAtDM(dm: MatrixD, iRow: Int, jCol: Int, nRows: Int, nCols: Int): ScalarD = 
        for {
            dm <- dm
            newId <- deltaLetScalar(DeltaScalar.ElementAtM(dm, iRow, jCol, nRows, nCols))
        } yield DeltaScalar.Val(newId)

    override def rowAtDM(dm: MatrixD, rowI: Int, nRows: Int, nCols: Int): RowVectorD = 
        for {
            dm <- dm
            newId <- deltaLetRowVector(DeltaRowVector.RowAtM(dm, rowI, nRows, nCols))
        } yield DeltaRowVector.Val(newId)

    override def elementAtDCV(dv: ColumnVectorD, index: Int, length: Int): ScalarD = 
        for {
            dv <- dv
            newId <- deltaLetScalar(DeltaScalar.ElementAtCV(dv, index, length))
        } yield DeltaScalar.Val(newId)

    override def elementAtDRV(dv: DeltaMonad[Double, DeltaRowVector[Double]], index: Int, length: Int): ScalarD =
        for {
            dv <- dv
            newId <- deltaLetScalar(DeltaScalar.ElementAtRV(dv, index, length))
        } yield DeltaScalar.Val(newId)

    override def elementWiseOpsM(m: Matrix, f: Scalar => Scalar): Matrix =
        createMatrix(
            new DenseMatrix(m.rows, m.cols, m.v.map(x => f(DualDeltaScalar(x, DeltaMonad.zeroS)).v).toArray),
            for {
                dm <- m.dv
                id <- deltaLetMatrix(DeltaMatrix.ElementWiseOps(m.v, dm, f))
            } yield DeltaMatrix.Val(id)
        )

    override def columnWiseOpsM(m: Matrix, f: ColumnVector => ColumnVector): Matrix =
        createMatrix(
            new DenseMatrix(m.rows, m.cols, m.v.toArray),
            for {
                dm <- m.dv
            } yield DeltaMatrix.ColumnWiseOps(m.v, dm, f)
        )

    override def rowWiseOpsM(m: Matrix, op: RowVector => RowVector): Matrix =
        val mv2 = m.v.copy
        for (r <- 0 until mv2.rows) {
            val c = op(createRowVector(mv2(r, ::), DeltaMonad.zeroRV))
            mv2(r, ::) := c.v
        }
        createMatrix(
            mv2, 
            for {
                dm <- m.dv
                newId <- deltaLetMatrix(DeltaMatrix.RowWiseOps(m.v, dm, op))
            } yield DeltaMatrix.Val(newId)
        )

    def elementWiseOpsMForward(m: Matrix, f: DualNumberScalar[Double] => DualNumberScalar[Double]): Matrix =
        createMatrix(
            new DenseMatrix(m.rows, m.cols, m.v.map(x => f(DualNumberScalar(x, 0.0)).v).toArray),
            for {
                dm <- m.dv
            } yield DeltaMatrix.ElementWiseOpsForward(m.v, dm, f)
        )

    override def elementWiseOpsCV(v: ColumnVector, f: Scalar => Scalar): ColumnVector =
        createColumnVector(
            DenseVector(v.v.toArray.map(x => f(DualDeltaScalar(x, DeltaMonad.zeroS)).v)),
            for {
                dv <- v.dv
            } yield DeltaColumnVector.ElementWiseOps(v.v, dv, f)
        )
       
    override def elementWiseOpsRV(v: RowVector, f: Scalar => Scalar): RowVector =
        createRowVector(
            Transpose(DenseVector(v.v.inner.toArray.map(x => f(DualDeltaScalar(x, DeltaMonad.zeroS)).v))),
            for {
                dv <- v.dv
            } yield DeltaRowVector.ElementWiseOps(v.v, dv, f)
        )

    def stackDRows(rows: RowVectorD*): MatrixD = 
        for {
            rows <- DeltaMonad.traverse(rows.toList)
            newId <- deltaLetMatrix(DeltaMatrix.StackDRows(rows))
        } yield DeltaMatrix.Val(newId)

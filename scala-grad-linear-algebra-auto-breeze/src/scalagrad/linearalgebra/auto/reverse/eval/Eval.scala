package scalagrad.linearalgebra.auto.reverse.eval

import scalagrad.api.ScalaGrad
import scalagrad.api.Dual
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
import scalagrad.linearalgebra.auto.reverse.delta.*
import scalagrad.linearalgebra.auto.reverse.dual.*

object Eval:

    def evalDeltas(id: Int, rhs: Deltas[Double], input: AccumulatedResult[Double]): AccumulatedResult[Double] = 
        rhs match
            case ds: DeltaScalar[Double] => 
                evalScalar(input.scalars(id), ds, input.copy(
                    scalars = input.scalars - id
                ))
            case dcv: DeltaColumnVector[Double] => 
                evalColumnVector(input.columnVectors(id), dcv, input.copy(
                    columnVectors = input.columnVectors - id
                ))
            case drv: DeltaRowVector[Double] =>
                evalRowVector(input.rowVectors(id), drv, input.copy(
                    rowVectors = input.rowVectors - id
                ))
            case dm: DeltaMatrix[Double] =>
                evalMatrix(input.matrices(id), dm, input.copy(
                    matrices = input.matrices - id
                ))

    def evalScalar(output: Double, delta: DeltaScalar[Double], input: AccumulatedResult[Double]): AccumulatedResult[Double] =
        delta match
            case DeltaScalar.Zero(_) => input
            case DeltaScalar.Val(id) =>
                input.copy(
                    scalars = 
                        input.scalars.updatedWith(id)(v => Some(
                            v.fold(output)(_ + output)
                        ))
                )
            case DeltaScalar.Let(id, rhs, body) =>
                val nextInput = evalScalar(output, body, input)
                evalDeltas(id, rhs, nextInput)
            case DeltaScalar.MultiplyVV(m1: DeltaRowVector[Double], m2: DeltaColumnVector[Double]) => ???
            case DeltaScalar.MultiplyRVDCV(v: Transpose[DenseVector[Double]], d: DeltaColumnVector[Double]) => 
                evalColumnVector(output * v.t, d, input)
            case DeltaScalar.MatrixDotDRVCV(d: DeltaRowVector[Double], v: DenseVector[Double]) =>
                evalRowVector((output * v).t, d, input)
            case DeltaScalar.Add(d1: DeltaScalar[Double], d2: DeltaScalar[Double]) =>
                evalScalar(output, d1, 
                    evalScalar(output, d2, input)
                )
            case DeltaScalar.ColumnDotProduct(v: Transpose[DenseVector[Double]], d: DeltaColumnVector[Double]) => 
                evalColumnVector(output * v.t, d, input)
            case DeltaScalar.RowDotProduct(d: DeltaRowVector[Double], v: DenseVector[Double]) =>
                evalRowVector((output * v).t, d, input)
            case DeltaScalar.Sum(d: DeltaColumnVector[Double], vectorLength: Int) =>
                evalColumnVector(
                    DenseVector.fill(vectorLength)(output), d, input
                )
            case DeltaScalar.SumM(d: DeltaMatrix[Double], nRows: Int, nCols: Int) =>
                evalMatrix(
                    DenseMatrix.fill(nRows, nCols)(output), d, input
                )
            case DeltaScalar.Sub(d1: DeltaScalar[Double], d2: DeltaScalar[Double]) =>
                evalScalar(output, d1, 
                    evalScalar(-output, d2, input)
                )
            case DeltaScalar.Div(d: DeltaScalar[Double], scale: Double) =>
                evalScalar(output / scale, d, input)
            case DeltaScalar.Scale(d: DeltaScalar[Double], scale: Double) =>
                evalScalar(output * scale, d, input)
            case DeltaScalar.ElementAtM(m: DeltaMatrix[Double], row: Int, col: Int, nRows: Int, nCols: Int) =>
                val id = m.asInstanceOf[DeltaMatrix.Val[Double]].id
                input.copy(
                    matrices = 
                        input.matrices.updatedWith(id)(ov => 
                            val v = ov.getOrElse(DenseMatrix.zeros[Double](nRows, nCols))
                            v(row, col) += output
                            Some(v)
                        )
                )
            case DeltaScalar.ElementAtCV(delta: DeltaColumnVector[Double], index: Int, deltaLength: Int) =>
                val id = delta.asInstanceOf[DeltaColumnVector.Val[Double]].id
                input.copy(
                    columnVectors = 
                        input.columnVectors.updatedWith(id)(ov => 
                            val v = ov.getOrElse(DenseVector.zeros[Double](deltaLength))
                            v(index) += output
                            Some(v)
                        )
                )
            case DeltaScalar.ElementAtRV(delta: DeltaRowVector[Double], index: Int, deltaLength: Int) =>
                import breeze.linalg.operators.HasOps.liftSlice
                input.copy(
                    rowVectors = 
                        input.rowVectors.updatedWith(index)(v => v.orElse(Some(
                            DenseVector.fill(deltaLength)(0.0).t
                        ).map(v =>
                            val vT = v.t
                            vT(index) += output
                            v
                        )))
                )

    def evalColumnVector(output: DenseVector[Double], delta: DeltaColumnVector[Double], input: AccumulatedResult[Double]): AccumulatedResult[Double] = 
        delta match
            case DeltaColumnVector.Zero(_) => input
            case DeltaColumnVector.Val(id) => 
                input.copy(
                    columnVectors = 
                        input.columnVectors.updatedWith(id)(v => Some(
                            v.fold(output)(_ + output)
                        ))
                )
            case DeltaColumnVector.Let(id, rhs, body) => 
                val nextInput = evalColumnVector(output, body, input)
                evalDeltas(id, rhs, nextInput)
            case DeltaColumnVector.Transpose(d) => 
                evalRowVector(output.t, d, input)
            case DeltaColumnVector.MatrixDotDMCV(dm, cv) =>
                evalMatrix(output * cv.t, dm, input)
            case DeltaColumnVector.MatrixDotMDCV(m, dcv) =>
                evalColumnVector(m.t * output, dcv, input)
            case DeltaColumnVector.MatrixDotDCVS(d, f) =>
                evalColumnVector(output * f, d, input)
            case DeltaColumnVector.MatrixDotCVDS(cv, ds) =>
                evalScalar(breeze.linalg.sum(output *:* cv), ds, input)
            case DeltaColumnVector.Div(d, f) =>
                evalColumnVector(output / f, d, input)
            case DeltaColumnVector.AddDCVDCV(dcv1, dcv2) => 
                evalColumnVector(output, dcv1, 
                    evalColumnVector(output, dcv2, input)
                )
            case DeltaColumnVector.AddVS(d, s) =>
                evalColumnVector(output, d, 
                    output.foldLeft(input)((input, outputScalar) => 
                        evalScalar(outputScalar, s, input)
                    )
                )
            case DeltaColumnVector.MinusVV(d1, d2) =>
                evalColumnVector(output, d1, 
                    evalColumnVector(-output, d2, input)
                )
            case DeltaColumnVector.MinusVS(dv, ds) =>
                evalColumnVector(output, dv, 
                    output.foldLeft(input)((input, outputScalar) => 
                        evalScalar(-outputScalar, ds, input)
                    )
                )
            case DeltaColumnVector.ElementWiseScale(v, d) =>
                evalColumnVector(output *:* v, d, input)
            case DeltaColumnVector.FromElements(values) =>
                values.zipWithIndex.foldLeft(input)((input, t) => 
                    val (dScalar, index) = t
                    evalScalar(output(index), dScalar, input)
                )
            case DeltaColumnVector.ElementWiseOps(v, d, op) =>
                import scalagrad.linearalgebra.auto.reverse.DeriverBreezeReversePlan.scalar2Scalar
                val dOps = scalar2Scalar.derive(op) // TODO fix: Here a new "context" is opened. But op can depend on Values of current context...
                evalColumnVector(output *:*  v.map(dOps), d, input)

    def evalRowVector(output: Transpose[DenseVector[Double]], delta: DeltaRowVector[Double], input: AccumulatedResult[Double]): AccumulatedResult[Double] = 
        delta match
            case DeltaRowVector.Zero(_) => input
            case DeltaRowVector.Val(id) =>
                input.copy(
                    rowVectors = 
                        input.rowVectors.updatedWith(id)(v => Some(
                            v.fold(output)(_ + output)
                        ))
                )
            case DeltaRowVector.Let(id, rhs, body) => 
                val nextInput = evalRowVector(output, body, input)
                evalDeltas(id, rhs, nextInput)
            case DeltaRowVector.Transpose(d) => 
                evalColumnVector(output.t, d, input)
            case DeltaRowVector.Scale(d, f) =>
                evalRowVector(output * f, d, input)
            case DeltaRowVector.MatrixDotRVDM(rv, dm) =>
                evalMatrix(rv.t * output, dm, input)
            case DeltaRowVector.MatrixDotDRVM(drv, m) =>
                evalRowVector(output * m.t, drv, input)
            case DeltaRowVector.MatrixDotRVDS(rv, ds) =>
                evalScalar(breeze.linalg.sum(output), ds, input)
            case DeltaRowVector.ElementWiseScale(v, d) =>
                evalRowVector(output *:* v, d, input)
            case DeltaRowVector.AddVV(drv1, drv2) => 
                evalRowVector(output, drv1, 
                    evalRowVector(output, drv2, input)
                )
            case DeltaRowVector.AddDRVDS(drv, ds) =>
                evalRowVector(output, drv, 
                    evalScalar(breeze.linalg.sum(output), ds, input)
                )
            case DeltaRowVector.MinusDRVDRV(drv1, drv2) =>
                evalRowVector(output, drv1, 
                    evalRowVector(-output, drv2, input)
                )
            case DeltaRowVector.MinusDRVDS(drv, ds) =>
                evalRowVector(output, drv, 
                    evalScalar(-breeze.linalg.sum(output), ds, input)
                )
            case DeltaRowVector.Div(drv, f) =>
                evalRowVector(output / f, drv, input)
            case DeltaRowVector.FromElements(values) =>
                values.zipWithIndex.foldLeft(input)((input, t) => 
                    val (dScalar, index) = t
                    evalScalar(output(index), dScalar, input)
                )
            case DeltaRowVector.RowAtM(dm, row, nRows, nCols) =>
                val id = dm.asInstanceOf[DeltaMatrix.Val[Double]].id
                input.copy(
                    matrices = 
                        input.matrices.updatedWith(id)(ov => 
                            val v = ov.getOrElse(DenseMatrix.zeros[Double](nRows, nCols))
                            v(row, ::) += output
                            Some(v)
                        )
                )
            case DeltaRowVector.ElementWiseOps(v, d, op) =>
                import scalagrad.linearalgebra.auto.reverse.DeriverBreezeReversePlan.scalar2Scalar
                val dOps = scalar2Scalar.derive(op)
                evalRowVector(output *:* v.inner.map(dOps).t, d, input)


    def evalMatrix(output: DenseMatrix[Double], delta: DeltaMatrix[Double], input: AccumulatedResult[Double]): AccumulatedResult[Double] = 
        delta match
            case DeltaMatrix.Zero(_) => input
            case DeltaMatrix.Val(id) => 
                input.copy(
                    matrices = 
                        input.matrices.updatedWith(id)(v => Some(
                            v.fold(output)(_ + output)
                        ))
                )
            case DeltaMatrix.Let(id, rhs, body) => 
                val nextInput = evalMatrix(output, body, input)
                evalDeltas(id, rhs, nextInput)
            case DeltaMatrix.AddDMDM(m1, m2) =>
                evalMatrix(output, m1, 
                    evalMatrix(output, m2, input)
                )
            case DeltaMatrix.AddDMDCV(m, dcv) =>
                evalMatrix(output, m, 
                    evalColumnVector(breeze.linalg.sum(output(*, ::)), dcv, input)
                )
            case DeltaMatrix.AddDMDRV(m, v) =>
                evalMatrix(output, m, 
                    evalRowVector(
                        breeze.linalg.sum(output(::, *)), v, input
                    )
                )
            case DeltaMatrix.AddDMDS(dm, ds) =>
                evalMatrix(output, dm, evalScalar(breeze.linalg.sum(output), ds, input))
            case DeltaMatrix.MinusDMDM(m1, m2) =>
                evalMatrix(output, m1, 
                    evalMatrix(-output, m2, input)
                )
            case DeltaMatrix.MinusDMDCV(dm, dcv) =>
                evalMatrix(output, dm, 
                    evalColumnVector(-breeze.linalg.sum(output(*, ::)), dcv, input)
                )
            case DeltaMatrix.MinusDMDRV(dm, drv) => 
                evalMatrix(output, dm, 
                    evalRowVector(-breeze.linalg.sum(output(::, *)), drv, input)
                )
            case DeltaMatrix.MinusDMDS(dm, ds) =>
                evalMatrix(output, dm, evalScalar(-breeze.linalg.sum(output), ds, input))
            case DeltaMatrix.Transpose(d) =>
                evalMatrix(output.t, d, input)
            case DeltaMatrix.MatrixDotMDM(m, dm) => 
                evalMatrix(m.t * output, dm, input)
            case DeltaMatrix.MatrixDotDCVRV(d, v) =>
                evalColumnVector(output * v.t, d, input)
            case DeltaMatrix.MatrixDotCVDRV(v, d) =>
                evalRowVector(v.t * output, d, input)
            case DeltaMatrix.MatrixDotMDS(m, ds) => 
                evalScalar(breeze.linalg.sum(output), ds, input)
            case DeltaMatrix.MatrixDotDMS(d, s) =>
                evalMatrix(output *:* s, d, input)
            case DeltaMatrix.ElementWiseScale(v, d) =>
                evalMatrix(output *:* v, d, input)
            case DeltaMatrix.MatrixDotDMM(dm, m) =>
                evalMatrix(output * m.t, dm, input)
            case DeltaMatrix.FromElements(values) =>
                values.zipWithIndex.foldLeft(input)((input, t) => 
                    val (dScalar, index) = t
                    val (jCol, iRow) = (index / output.rows, index % output.rows)
                    evalScalar(output(iRow, jCol), dScalar, input)
                )
            case DeltaMatrix.StackDRows(rows) =>
                rows.zipWithIndex.foldLeft(input)((input, t) => 
                    val (dRowVector, index) = t
                    evalRowVector(output(index, ::), dRowVector, input)
                )
            case DeltaMatrix.Div(dm, s) =>
                evalMatrix(output / s, dm, input)
            case DeltaMatrix.ElementWiseOps(v, d, op) =>
                import scalagrad.linearalgebra.auto.reverse.DeriverBreezeReversePlan.scalar2Scalar
                val dOps = scalar2Scalar.derive(op)
                evalMatrix(output *:* v.map(dOps), d, input)
            case DeltaMatrix.RowWiseOps(v, d, op) => 
                import scalagrad.linearalgebra.auto.reverse.DeriverBreezeReversePlan.rowVector2RowVector
                val dOps = rowVector2RowVector.derive(op)
                val nextOutput = DenseMatrix.zeros[Double](output.rows, v.cols)
                for (r <- 0 until v.rows) {
                    nextOutput(r, ::) := (dOps(v(r, ::)) * output(r, ::).t).t
                }
                evalMatrix(nextOutput, d, input)
            case DeltaMatrix.RowWiseOpsForward(v, d, op) => 
                import scalagrad.linearalgebra.auto.forward.DeriverBreezeForwardPlan.rowVector2RowVector
                val dOps = rowVector2RowVector.derive(op)
                val nextOutput = DenseMatrix.zeros[Double](output.rows, v.cols)
                for (r <- 0 until v.rows) {
                    nextOutput(r, ::) := (dOps(v(r, ::)) * output(r, ::).t).t
                }
                evalMatrix(nextOutput, d, input)
            case DeltaMatrix.RowWiseOpsManual(v, d, dOp) => 
                val nextOutput = DenseMatrix.zeros[Double](output.rows, v.cols)
                for (r <- 0 until v.rows) {
                    nextOutput(r, ::) := (dOp(v(r, ::)) * output(r, ::).t).t
                }
                evalMatrix(nextOutput, d, input)
            case DeltaMatrix.ElementWiseOpsForward(v, d, op) =>
                import scalagrad.linearalgebra.auto.forward.DeriverBreezeForwardPlan.scalar2Scalar
                val dOps = scalar2Scalar.derive(op)
                evalMatrix(output *:* v.map(dOps), d, input)

    case class AccumulatedResult[P](
        scalars: Map[Int, P],
        columnVectors: Map[Int, DenseVector[P]],
        rowVectors: Map[Int, Transpose[DenseVector[P]]],
        matrices: Map[Int, DenseMatrix[P]]
    )

    object AccumulatedResult:
        def empty[P]: AccumulatedResult[P] = AccumulatedResult[P](Map(), Map(), Map(), Map())

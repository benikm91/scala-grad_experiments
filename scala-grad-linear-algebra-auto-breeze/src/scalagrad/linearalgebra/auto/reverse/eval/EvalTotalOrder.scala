package scalagrad.linearalgebra.auto.reverse.eval

import scalagrad.api.ScalaGrad
import scalagrad.api.Dual
import scalagrad.auto.forward.dual.DualNumber
import scalagrad.fractional.auto.dual.DualIsFractional.given
import scalagrad.auto.reverse.dual.DualDelta
import breeze.linalg.{Vector as _, *}
import breeze.linalg.operators.*
import breeze.linalg.support.*
import scalagrad.api.Deriver
import scalagrad.api.DeriverPlan

import scala.reflect.ClassTag
import scala.math.Fractional
import scala.runtime.Tuples
import scalagrad.auto.forward.dual.DualNumber
import scalagrad.linearalgebra.auto.reverse.delta.*
import scalagrad.linearalgebra.auto.reverse.dual.*

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

// TODO change to LinearAlgebraOps
object EvalTotalOrder:
    
    enum EvalStepResult:
        case EvalStepScalarResult(output: Double, ds: DeltaScalar[Double])
        case EvalStepColumnVectorResult(output: DenseVector[Double], dcv: DeltaColumnVector[Double])
        case EvalStepRowVectorResult(output: Transpose[DenseVector[Double]], drv: DeltaRowVector[Double])
        case EvalStepMatrixResult(output: DenseMatrix[Double], dm: DeltaMatrix[Double])

    import EvalStepResult.*

    def eval(
        start: EvalStepResult,
        endIndex: Int,
    ): Results[Double] = 
        
        var results = Results.empty[Double]

        if (endIndex == -1) return results

        val intermediateResults = Array.ofDim[EvalStepResult](endIndex + 1)
        intermediateResults(endIndex) = start

        var i = endIndex
        while(i >= 0) {
            intermediateResults(i) match {
                case null => // skip
                case EvalStepScalarResult(output, ds) =>
                    ds.index = -1
                    results = evalScalarStep(output, ds, intermediateResults, endIndex, results)
                case EvalStepColumnVectorResult(output, dcv) =>
                    dcv.index = -1
                    results = evalColumnVectorStep(output, dcv, intermediateResults, endIndex, results)
                case EvalStepRowVectorResult(output, drv) =>
                    drv.index = -1
                    results = evalRowVectorStep(output, drv, intermediateResults, endIndex, results)
                case EvalStepMatrixResult(output, dm) =>
                    dm.index = -1
                    results = evalMatrixStep(output, dm, intermediateResults, endIndex, results)
            }
            intermediateResults(i) = null // clear
            i -= 1
        }

        results

    def evalScalar(output: Double, delta: DeltaScalar[Double]): Results[Double] =
        assert(delta.index != -1)
        eval(EvalStepScalarResult(output, delta), delta.index)

    def evalRowVector(output: Transpose[DenseVector[Double]], delta: DeltaRowVector[Double]): Results[Double] = 
        assert(delta.index != -1)
        eval(EvalStepRowVectorResult(output, delta), delta.index)

    def evalScalarStep(
        output: Double, 
        delta: DeltaScalar[Double],
        intermediateResults: Array[EvalStepResult],
        endIndex: Int,
        results: Results[Double],
    ): Results[Double] =
        if (delta.index != -1) then
            val index = delta.index
            intermediateResults(index) =
                Option(intermediateResults(index).asInstanceOf[EvalStepScalarResult]).fold(
                    EvalStepScalarResult(output, delta)
                )(x =>
                    assert(x.ds.index == delta.index)
                    x.copy(output = x.output + output)
                )
            results
        else 
            delta match
                case DeltaScalar.Zero(_) => results
                case DeltaScalar.Val(id) => 
                    results.copy(
                        scalars = 
                            results.scalars.updatedWith(id)(v => Some(
                                v.fold(output)(_ + output)
                            ))
                    )
                case DeltaScalar.MultiplyVV(m1: DeltaRowVector[Double], m2: DeltaColumnVector[Double]) => ???
                case DeltaScalar.MultiplyRVDCV(v: Transpose[DenseVector[Double]], d: DeltaColumnVector[Double]) => 
                    evalColumnVectorStep(output * v.t, d, intermediateResults, endIndex, results)
                case DeltaScalar.MatrixDotDRVCV(d: DeltaRowVector[Double], v: DenseVector[Double]) =>
                    evalRowVectorStep((output * v).t, d, intermediateResults, endIndex, results)
                case DeltaScalar.Add(d1: DeltaScalar[Double], d2: DeltaScalar[Double]) =>
                    evalScalarStep(output, d1, intermediateResults, endIndex,
                        evalScalarStep(output, d2, intermediateResults, endIndex, results)
                    )
                case DeltaScalar.ColumnDotProduct(v: Transpose[DenseVector[Double]], d: DeltaColumnVector[Double]) => 
                    evalColumnVectorStep(output * v.t, d, intermediateResults, endIndex, results)
                case DeltaScalar.RowDotProduct(d: DeltaRowVector[Double], v: DenseVector[Double]) =>
                    evalRowVectorStep((output * v).t, d, intermediateResults, endIndex, results)
                case DeltaScalar.Sum(d: DeltaColumnVector[Double], vectorLength: Int) =>
                    evalColumnVectorStep(
                        DenseVector.fill(vectorLength)(output), d, intermediateResults, endIndex, results
                    )
                case DeltaScalar.SumM(d: DeltaMatrix[Double], nRows: Int, nCols: Int) =>
                    evalMatrixStep(
                        DenseMatrix.fill(nRows, nCols)(output), d, intermediateResults, endIndex, results
                    )
                case DeltaScalar.Sub(d1: DeltaScalar[Double], d2: DeltaScalar[Double]) =>
                    evalScalarStep(output, d1, intermediateResults, endIndex, 
                        evalScalarStep(-output, d2, intermediateResults, endIndex, results)
                    )
                case DeltaScalar.Div(d: DeltaScalar[Double], scale: Double) =>
                    evalScalarStep(output / scale, d, intermediateResults, endIndex, results)
                case DeltaScalar.Scale(scale: Double, ds: DeltaScalar[Double]) =>
                    evalScalarStep(output * scale, ds, intermediateResults, endIndex, results)
                case DeltaScalar.ElementAtM(dm: DeltaMatrix[Double], row: Int, col: Int, nRows: Int, nCols: Int) =>
                    val index = dm.index
                    lazy val newOutput = {
                        val newOutput = DenseMatrix.zeros[Double](nRows, nCols)
                        newOutput(row, col) = output
                        newOutput
                    }
                    if (index == -1) {
                        // If dm is a Val, then we can directly update the result
                        assert(dm.isInstanceOf[DeltaMatrix.Val[Double]])
                        evalMatrixStep(newOutput, dm, intermediateResults, endIndex, results)
                    } else {
                        // Store change in intermediateResults
                        intermediateResults(index) = Option(intermediateResults(index).asInstanceOf[EvalStepMatrixResult]).fold(
                            EvalStepMatrixResult(newOutput, dm)
                        )(x =>
                            assert(x.dm.index == dm.index)
                            val existingOutput = x.output.copy
                            existingOutput(row, col) += output
                            x.copy(output = existingOutput)
                        )
                        results
                    }
                case DeltaScalar.ElementAtCV(dcv: DeltaColumnVector[Double], i: Int, deltaLength: Int) =>
                    val index = dcv.index
                    lazy val newOutput = {
                        val newOutput = DenseVector.zeros[Double](deltaLength)
                        newOutput(i) = output
                        newOutput
                    }
                    if (index == -1) {
                        // If dm is a Val, then we can directly update the result
                        assert(dcv.isInstanceOf[DeltaColumnVector.Val[Double]])
                        evalColumnVectorStep(newOutput, dcv, intermediateResults, endIndex, results)
                    } else {
                        intermediateResults(index) = 
                            Option(intermediateResults(index).asInstanceOf[EvalStepColumnVectorResult]).fold(
                                EvalStepColumnVectorResult(newOutput, dcv)
                            )(x =>
                                assert(x.dcv.index == dcv.index)
                                val existingOutput = x.output.copy
                                existingOutput(i) += output
                                x.copy(output = existingOutput)
                            )
                        results
                    }
                case DeltaScalar.ElementAtRV(delta: DeltaRowVector[Double], index: Int, deltaLength: Int) =>
                    import breeze.linalg.operators.HasOps.liftSlice
                    ???

    def evalColumnVectorStep(
        output: DenseVector[Double], 
        delta: DeltaColumnVector[Double],
        intermediateResults: Array[EvalStepResult],
        endIndex: Int,
        results: Results[Double],
    ): Results[Double] = 
        if (delta.index != -1) then
            val index = delta.index
            intermediateResults(index) =
                Option(intermediateResults(index).asInstanceOf[EvalStepColumnVectorResult]).fold(
                    EvalStepColumnVectorResult(output, delta)
                )(x =>
                    assert(x.dcv.index == delta.index)
                    x.copy(output = x.output + output)
                )
            results
        else
            delta match
                case DeltaColumnVector.Zero(_) => results
                case DeltaColumnVector.Val(id) => 
                    results.copy(
                        columnVectors = 
                            results.columnVectors.updatedWith(id)(v => Some(
                                v.fold(output)(_ + output)
                            ))
                    )
                case DeltaColumnVector.Transpose(d) => 
                    evalRowVectorStep(output.t, d, intermediateResults, endIndex, results)
                case DeltaColumnVector.MatrixDotDMCV(dm, cv) =>
                    evalMatrixStep(output * cv.t, dm, intermediateResults, endIndex, results)
                case DeltaColumnVector.MatrixDotMDCV(m, dcv) =>
                    evalColumnVectorStep(m.t * output, dcv, intermediateResults, endIndex, results)
                case DeltaColumnVector.MatrixDotDCVS(d, f) =>
                    evalColumnVectorStep(output * f, d, intermediateResults, endIndex, results)
                case DeltaColumnVector.MatrixDotCVDS(cv, ds) =>
                    evalScalarStep(breeze.linalg.sum(output *:* cv), ds, intermediateResults, endIndex, results)
                case DeltaColumnVector.Div(d, f) =>
                    evalColumnVectorStep(output / f, d, intermediateResults, endIndex, results)
                case DeltaColumnVector.AddDCVDCV(dcv1, dcv2) => 
                    evalColumnVectorStep(output, dcv1, intermediateResults, endIndex, 
                        evalColumnVectorStep(output, dcv2, intermediateResults, endIndex, results)
                    )
                case DeltaColumnVector.AddVS(d, s) =>
                    evalColumnVectorStep(output, d, intermediateResults, endIndex, 
                        output.foldLeft(results)((results, outputScalar) => 
                            evalScalarStep(outputScalar, s, intermediateResults, endIndex, results)
                        )
                    )
                case DeltaColumnVector.MinusVV(d1, d2) =>
                    evalColumnVectorStep(output, d1, intermediateResults, endIndex,
                        evalColumnVectorStep(-output, d2, intermediateResults, endIndex, results)
                    )
                case DeltaColumnVector.MinusVS(dv, ds) =>
                    evalColumnVectorStep(output, dv, intermediateResults, endIndex, 
                        output.foldLeft(results)((results, outputScalar) => 
                            evalScalarStep(-outputScalar, ds, intermediateResults, endIndex, results)
                        )
                    )
                case DeltaColumnVector.ElementWiseScale(v, d) =>
                    evalColumnVectorStep(output *:* v, d, intermediateResults, endIndex, results)
                case DeltaColumnVector.FromElements(values) =>
                    values.zipWithIndex.foldLeft(results)((results, t) => 
                        val (dScalar, index) = t
                        evalScalarStep(output(index), dScalar, intermediateResults, endIndex, results)
                    )
                case DeltaColumnVector.ElementWiseOps(v, d, op) =>
                    import scalagrad.linearalgebra.auto.reverse.DeriverBreezeReversePlan.scalar2Scalar
                    val dOps = scalar2Scalar.derive(op) // TODO fix: Here a new "context" is opened. But op can depend on Values of current context...
                    evalColumnVectorStep(output *:*  v.map(dOps), d, intermediateResults, endIndex, results)

    def evalRowVectorStep(
        output: Transpose[DenseVector[Double]], 
        delta: DeltaRowVector[Double],
        intermediateResults: Array[EvalStepResult],
        endIndex: Int,
        results: Results[Double],
    ): Results[Double] = 
        if (delta.index != -1) then
            val index = delta.index
            intermediateResults(index) =
                Option(intermediateResults(index).asInstanceOf[EvalStepRowVectorResult]).fold(
                    EvalStepRowVectorResult(output, delta)
                )(x =>
                    assert(x.drv.index == delta.index)
                    x.copy(output = x.output + output)
                )
            results
        else
            delta match
                case DeltaRowVector.Zero(_) => results
                case DeltaRowVector.Val(id) => 
                    results.copy(
                        rowVectors = 
                            results.rowVectors.updatedWith(id)(v => Some(
                                v.fold(output)(_ + output)
                            ))
                    )
                case DeltaRowVector.Transpose(dcv) => 
                    evalColumnVectorStep(output.t, dcv, intermediateResults, endIndex, results)
                case DeltaRowVector.Scale(d, f) =>
                    evalRowVectorStep(output * f, d, intermediateResults, endIndex, results)
                case DeltaRowVector.MatrixDotRVDM(rv, dm) =>
                    evalMatrixStep(rv.t * output, dm, intermediateResults, endIndex, results)
                case DeltaRowVector.MatrixDotDRVM(drv, m) =>
                    evalRowVectorStep(output * m.t, drv, intermediateResults, endIndex, results)
                case DeltaRowVector.MatrixDotRVDS(rv, ds) =>
                    evalScalarStep(breeze.linalg.sum(output), ds, intermediateResults, endIndex, results)
                case DeltaRowVector.ElementWiseScale(v, d) =>
                    evalRowVectorStep(output *:* v, d, intermediateResults, endIndex, results)
                case DeltaRowVector.AddVV(drv1, drv2) => 
                    evalRowVectorStep(output, drv1, intermediateResults, endIndex, 
                        evalRowVectorStep(output, drv2, intermediateResults, endIndex, results)
                    )
                case DeltaRowVector.AddDRVDS(drv, ds) =>
                    evalRowVectorStep(output, drv, intermediateResults, endIndex, 
                        evalScalarStep(breeze.linalg.sum(output), ds, intermediateResults, endIndex, results)
                    )
                case DeltaRowVector.MinusDRVDRV(drv1, drv2) =>
                    evalRowVectorStep(output, drv1, intermediateResults, endIndex, 
                        evalRowVectorStep(-output, drv2, intermediateResults, endIndex, results)
                    )
                case DeltaRowVector.MinusDRVDS(drv, ds) =>
                    evalRowVectorStep(output, drv, intermediateResults, endIndex, 
                        evalScalarStep(-breeze.linalg.sum(output), ds, intermediateResults, endIndex, results)
                    )
                case DeltaRowVector.Div(drv, f) =>
                    evalRowVectorStep(output / f, drv, intermediateResults, endIndex, results)
                case DeltaRowVector.FromElements(values) =>
                    values.zipWithIndex.foldLeft(results)((results, t) => 
                        val (dScalar, index) = t
                        evalScalarStep(output(index), dScalar, intermediateResults, endIndex, results)
                    )
                case DeltaRowVector.RowAtM(dm, row, nRows, nCols) =>
                    val index = dm.index
                    lazy val newOutput = {
                        val newOutput = DenseMatrix.zeros[Double](nRows, nCols)
                        newOutput(row, ::) := output
                        newOutput
                    }
                    if (index == -1) {
                        // If dm is a Val, then we can directly update the result
                        assert(dm.isInstanceOf[DeltaMatrix.Val[Double]])
                        evalMatrixStep(newOutput, dm, intermediateResults, endIndex, results)
                    } else {
                        intermediateResults(index) =
                            Option(intermediateResults(index).asInstanceOf[EvalStepMatrixResult]).fold(
                                EvalStepMatrixResult(newOutput, dm)
                            )(x =>
                                assert(x.dm.index == dm.index)
                                val newOutput = x.output.copy
                                newOutput(row, ::) += output
                                x.copy(output = newOutput)
                            )
                        results
                    }
                case DeltaRowVector.ElementWiseOps(v, d, op) =>
                    import scalagrad.linearalgebra.auto.reverse.DeriverBreezeReversePlan.scalar2Scalar
                    val dOps = scalar2Scalar.derive(op)
                    evalRowVectorStep(output *:* v.inner.map(dOps).t, d, intermediateResults, endIndex, results)


    def evalMatrixStep(
        output: DenseMatrix[Double], 
        delta: DeltaMatrix[Double],
        intermediateResults: Array[EvalStepResult],
        endIndex: Int,
        results: Results[Double],
    ): Results[Double] = 
        if (delta.index != -1) then
            val index = delta.index
            intermediateResults(index) =
                Option(intermediateResults(index).asInstanceOf[EvalStepMatrixResult]).fold(
                    EvalStepMatrixResult(output, delta)
                )(x =>
                    assert(x.dm.index == delta.index)
                    x.copy(output = x.output + output)
                )
            results
        else
            delta match
                case DeltaMatrix.Zero(_) => results
                case DeltaMatrix.Val(id) => 
                    results.copy(
                        matrices =
                            results.matrices.updatedWith(id)(v => Some(
                                v.fold(output)(_ + output)
                            ))
                    )
                case DeltaMatrix.AddDMDM(m1, m2) =>
                    evalMatrixStep(output, m1, intermediateResults, endIndex, 
                        evalMatrixStep(output, m2, intermediateResults, endIndex, results)
                    )
                case DeltaMatrix.AddDMDCV(m, dcv) =>
                    evalMatrixStep(output, m, intermediateResults, endIndex, 
                        evalColumnVectorStep(breeze.linalg.sum(output(*, ::)), dcv, intermediateResults, endIndex, results)
                    )
                case DeltaMatrix.AddDMDRV(m, v) =>
                    evalMatrixStep(output, m, intermediateResults, endIndex,
                        evalRowVectorStep(
                            breeze.linalg.sum(output(::, *)), v, intermediateResults, endIndex, results
                        )
                    )
                case DeltaMatrix.AddDMDS(dm, ds) =>
                    evalMatrixStep(output, dm, intermediateResults, endIndex, evalScalarStep(breeze.linalg.sum(output), ds, intermediateResults, endIndex, results))
                case DeltaMatrix.MinusDMDM(m1, m2) =>
                    evalMatrixStep(output, m1, intermediateResults, endIndex,
                        evalMatrixStep(-output, m2, intermediateResults, endIndex, results)
                    )
                case DeltaMatrix.MinusDMDCV(dm, dcv) =>
                    evalMatrixStep(output, dm, intermediateResults, endIndex,
                        evalColumnVectorStep(-breeze.linalg.sum(output(*, ::)), dcv, intermediateResults, endIndex, results)
                    )
                case DeltaMatrix.MinusDMDRV(dm, drv) => 
                    evalMatrixStep(output, dm, intermediateResults, endIndex,
                        evalRowVectorStep(-breeze.linalg.sum(output(::, *)), drv, intermediateResults, endIndex, results)
                    )
                case DeltaMatrix.MinusDMDS(dm, ds) =>
                    evalMatrixStep(output, dm, intermediateResults, endIndex, evalScalarStep(-breeze.linalg.sum(output), ds, intermediateResults, endIndex, results))
                case DeltaMatrix.Transpose(d) =>
                    evalMatrixStep(output.t, d, intermediateResults, endIndex, results)
                case DeltaMatrix.MatrixDotMDM(m, dm) => 
                    evalMatrixStep(m.t * output, dm, intermediateResults, endIndex, results)
                case DeltaMatrix.MatrixDotDCVRV(d, v) =>
                    evalColumnVectorStep(output * v.t, d, intermediateResults, endIndex, results)
                case DeltaMatrix.MatrixDotCVDRV(v, d) =>
                    evalRowVectorStep(v.t * output, d, intermediateResults, endIndex, results)
                case DeltaMatrix.MatrixDotMDS(m, ds) => 
                    evalScalarStep(breeze.linalg.sum(output), ds, intermediateResults, endIndex, results)
                case DeltaMatrix.MatrixDotDMS(d, s) =>
                    evalMatrixStep(output *:* s, d, intermediateResults, endIndex, results)
                case DeltaMatrix.ElementWiseScale(v, d) =>
                    evalMatrixStep(output *:* v, d, intermediateResults, endIndex, results)
                case DeltaMatrix.MatrixDotDMM(dm, m) =>
                    evalMatrixStep(output * m.t, dm, intermediateResults, endIndex, results)
                case DeltaMatrix.FromElements(values) =>
                    values.zipWithIndex.foldLeft(results)((results, t) => 
                        val (dScalar, index) = t
                        val (jCol, iRow) = (index / output.rows, index % output.rows)
                        evalScalarStep(output(iRow, jCol), dScalar, intermediateResults, endIndex, results)
                    )
                case DeltaMatrix.StackDRows(rows) =>
                    rows.zipWithIndex.foldLeft(results)((results, t) => 
                        val (dRowVector, index) = t
                        evalRowVectorStep(output(index, ::), dRowVector, intermediateResults, endIndex, results)
                    )
                case DeltaMatrix.Div(dm, s) =>
                    evalMatrixStep(output / s, dm, intermediateResults, endIndex, results)
                case DeltaMatrix.ElementWiseOps(v, d, op) =>
                    import scalagrad.linearalgebra.auto.reverse.DeriverBreezeReversePlan.scalar2Scalar
                    val dOps = scalar2Scalar.derive(op)
                    evalMatrixStep(output *:* v.map(dOps), d, intermediateResults, endIndex, results)
                case DeltaMatrix.RowWiseOps(v, d, op) => 
                    import scalagrad.linearalgebra.auto.reverse.DeriverBreezeReversePlan.rowVector2RowVector
                    val dOps = rowVector2RowVector.derive(op)
                    val nextOutput = DenseMatrix.zeros[Double](output.rows, v.cols)
                    for (r <- 0 until v.rows) {
                        nextOutput(r, ::) := (dOps(v(r, ::)) * output(r, ::).t).t
                    }
                    evalMatrixStep(nextOutput, d, intermediateResults, endIndex, results)
                case DeltaMatrix.RowWiseOpsForward(v, d, op) => 
                    import scalagrad.linearalgebra.auto.forward.DeriverBreezeForwardPlan.rowVector2RowVector
                    val dOps = rowVector2RowVector.derive(op)
                    val nextOutput = DenseMatrix.zeros[Double](output.rows, v.cols)
                    for (r <- 0 until v.rows) {
                        nextOutput(r, ::) := (dOps(v(r, ::)) * output(r, ::).t).t
                    }
                    evalMatrixStep(nextOutput, d, intermediateResults, endIndex, results)
                case DeltaMatrix.RowWiseOpsManual(v, d, dOp) => ???
                case DeltaMatrix.ElementWiseOpsForward(v, dm, op) => 
                    import scalagrad.linearalgebra.auto.forward.DeriverBreezeForwardPlan.scalar2Scalar
                    val dOps = scalar2Scalar.derive(op)
                    evalMatrixStep(output *:* v.map(dOps), dm, intermediateResults, endIndex, results)
                    

    case class Results[P](
        scalars: Map[Int, P],
        columnVectors: Map[Int, DenseVector[P]],
        rowVectors: Map[Int, Transpose[DenseVector[P]]],
        matrices: Map[Int, DenseMatrix[P]]
    )

    object Results:
        def empty[P]: Results[P] = Results[P](Map(), Map(), Map(), Map())

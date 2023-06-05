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
            case DeltaScalar.Sub(d1: DeltaScalar[Double], d2: DeltaScalar[Double]) =>
                evalScalar(output, d1, 
                    evalScalar(-output, d2, input)
                )
            case DeltaScalar.Div(d: DeltaScalar[Double], scale: Double) =>
                evalScalar(output / scale, d, input)
            case DeltaScalar.Scale(d: DeltaScalar[Double], scale: Double) =>
                evalScalar(output * scale, d, input)

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
            case DeltaColumnVector.MatrixDot(d, v) =>
                evalMatrix(output * v.t, d, input)
            case DeltaColumnVector.MatrixDot2(v, d) =>
                evalColumnVector(v.t * output, d, input)
            case DeltaColumnVector.AddVV(d1, d2) => 
                evalColumnVector(output, d1, 
                    evalColumnVector(output, d2, input)
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
            case DeltaColumnVector.ElementWiseScale(v, d) =>
                evalColumnVector(output *:* v, d, input)


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
            case DeltaRowVector.MatrixDot(v, d) =>
                evalMatrix(v.t * output, d, input)
            case DeltaRowVector.MatrixDot2(d, v) =>
                evalRowVector(output * v, d, input)
            case DeltaRowVector.AddVV(d1, d2) => 
                evalRowVector(output, d1, 
                    evalRowVector(output, d2, input)
                )


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
            case DeltaMatrix.AddDMDCV(m, v) =>
                evalMatrix(output, m, 
                    output(*, ::).foldLeft(input)((input, outputVector) => 
                        evalColumnVector(outputVector, v, input)
                    )
                )
            case DeltaMatrix.Transpose(d) =>
                evalMatrix(output.t, d, input)
            case DeltaMatrix.MatrixDotMDM(m, d) => 
                evalMatrix(m.t * output, d, input)
            case DeltaMatrix.MatrixDotDCVRV(d, v) =>
                evalColumnVector(output * v.t, d, input)
            case DeltaMatrix.MatrixDotCVDRV(v, d) =>
                evalRowVector(v.t * output, d, input)
            case DeltaMatrix.ElementWiseScale(v, d) =>
                evalMatrix(output *:* v, d, input)
            case DeltaMatrix.MatrixDotDMM(d, m) =>
                evalMatrix(output * m.t, d, input)

    case class AccumulatedResult[P](
        scalars: Map[Int, P],
        columnVectors: Map[Int, DenseVector[P]],
        rowVectors: Map[Int, Transpose[DenseVector[P]]],
        matrices: Map[Int, DenseMatrix[P]]
    )

    object AccumulatedResult:
        def empty[P]: AccumulatedResult[P] = AccumulatedResult[P](Map(), Map(), Map(), Map())

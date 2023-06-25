package scalagrad.api.linearalgebra

import scala.annotation.targetName
import scala.math.Fractional.Implicits._


trait BasicDualMatrixAlgebra[Scalar, ColumnVector, RowVector, Matrix] 
    extends LengthOps[ColumnVector, RowVector, Matrix]
    // with MatrixOps[Matrix, Scalar]
    with TransposeOps[ColumnVector, RowVector, Matrix]
    with NegateOps[Scalar, ColumnVector, RowVector, Matrix]
    with ScalarInvertOps[Scalar]
    with BasicOps[Scalar, ColumnVector, RowVector, Matrix]
    // with SumOps[Scalar, ColumnVector, RowVector, Matrix]
    with AccessOps[Scalar, ColumnVector, RowVector, Matrix]
    with CreateOps[Scalar, ColumnVector, RowVector, Matrix]


trait ElementWiseDerivativeOps[Scalar, ColumnVector, RowVector, Matrix]:

    def elementWiseOpM(m: Matrix, op: Scalar => Scalar, dOp: Scalar => Scalar): Matrix
    
    def columnWiseOpsM(m: Matrix, op: ColumnVector => ColumnVector, dOp: ColumnVector => Matrix): Matrix

    def rowWiseOpsM(m: Matrix, op: RowVector => RowVector, dOp: RowVector => Matrix): Matrix

    def elementWiseOpCV(cv: ColumnVector, op: Scalar => Scalar, dOp: Scalar => Scalar): ColumnVector
    
    def elementWiseOpRV(rv: RowVector, op: Scalar => Scalar, dOp: Scalar => Scalar): RowVector

    extension (m: Matrix)
        @targetName("elementWiseOpM_Op")
        def mapElements(op: Scalar => Scalar, dOp: Scalar => Scalar): Matrix = elementWiseOpM(m, op, dOp)
        @targetName("columnWiseOpsM_Op")
        def mapColumns(op: ColumnVector => ColumnVector, dOp: ColumnVector => Matrix): Matrix = columnWiseOpsM(m, op, dOp)
        @targetName("rowWiseOpsM_Op")
        def mapRows(op: RowVector => RowVector, dOp: RowVector => Matrix): Matrix = rowWiseOpsM(m, op, dOp)

    extension (cv: ColumnVector)
        @targetName("elementWiseOpCV_Op")
        def mapElements(op: Scalar => Scalar, dOp: Scalar => Scalar): ColumnVector = elementWiseOpCV(cv, op, dOp)

    extension (rv: RowVector)
        @targetName("elementWiseOpRV_Op")
        def mapElements(op: Scalar => Scalar, dOp: Scalar => Scalar): RowVector = elementWiseOpRV(rv, op, dOp)
    

trait DualMatrixAlgebra[
    DualScalar[_, _], DualColumnVector[_, _], DualRowVector[_, _], DualMatrix[_, _],
    PScalar, PColumnVector, PRowVector, PMatrix,
    DScalar, DColumnVector, DRowVector, DMatrix,
] extends BasicDualMatrixAlgebra[
    DualScalar[PScalar, DScalar], 
    DualColumnVector[PColumnVector, DColumnVector], 
    DualRowVector[PRowVector, DRowVector], 
    DualMatrix[PMatrix, DMatrix]
] with ElementWiseDerivativeOps[PScalar, PColumnVector, PRowVector, PMatrix]:

    private type Matrix = DualMatrix[PMatrix, DMatrix]
    private type ColumnVector = DualColumnVector[PColumnVector, DColumnVector]
    private type RowVector = DualRowVector[PRowVector, DRowVector]
    private type Scalar = DualScalar[PScalar, DScalar]

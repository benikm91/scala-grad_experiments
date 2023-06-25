package scalagrad.showcase.deeplearning

import scalagrad.linearalgebra.auto.reverse.delta.Deltas
import scalagrad.linearalgebra.auto.forward.dual.*

import breeze.linalg.*

import scalagrad.api.linearalgebra.LinearAlgebraOps
import scalagrad.linearalgebra.auto.reverse.delta.DeltaScalar
import scalagrad.linearalgebra.auto.forward.BreezeVectorAlgebraForDualNumberDouble

type DualTuple[T <: Tuple] = T match
    case DualNumberScalar[Double] *: t => DualTuple[t]
    case DualNumberColumnVector[Double] *: t => DualTuple[t]
    case DualNumberRowVector[Double] *: t => DualTuple[t]
    case DualNumberMatrix[Double] *: t => DualTuple[t]
    case EmptyTuple => DummyImplicit

type FromDuals[T <: Tuple] <: Tuple = T match
  case (head *: tail) => FromDual[head] *: FromDuals[tail]
  case EmptyTuple     => EmptyTuple

type FromDual[T] = T match
  case DualNumberScalar[Double]         => Double
  case DualNumberColumnVector[Double]   => DenseVector[Double]
  case DualNumberRowVector[Double]      => Transpose[DenseVector[Double]]
  case DualNumberMatrix[Double]         => DenseMatrix[Double]

type ToDual[T] = T match
  case Double                           => DualNumberScalar[Double]
  case DenseVector[Double]              => DualNumberColumnVector[Double]
  case Transpose[DenseVector[Double]]   => DualNumberRowVector[Double]
  case DenseMatrix[Double ]             => DualNumberMatrix[Double]

@main
def aPlayground = 

    def derive[T <: Tuple : DualTuple](f: T => DualNumberScalar[Double])(t: FromDuals[T]): FromDuals[T] = 
        def toZeros(t: FromDuals[T]): T = 
            t.map[[X] =>> ToDual[X]]([T] => (t: T) => t match {
                case x: Double => DualNumberScalar[Double](x, 0.0)
                case x: DenseVector[Double] => DualNumberColumnVector(x, DenseVector.zeros[Double](x.length))
                case x: Transpose[DenseVector[Double]] => DualNumberRowVector(x, DenseVector.zeros[Double](x.inner.length).t)
                case x: DenseMatrix[Double] => DualNumberMatrix(x, DenseMatrix.zeros[Double](x.rows, x.cols))
            }).asInstanceOf[T]
        def forwardPlan(zeros: T): FromDuals[T] = 
            val indices = (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22)
            val zerosWithIndex = zeros.zip(indices)
            def oneHotDenseVector(size: Int, i: Int): DenseVector[Double] =
                val res = DenseVector.zeros[Double](size)
                res(i) = 1.0
                res
            def oneHotDenseMatrix(mrows: Int, mcols: Int, i: Int): DenseMatrix[Double] = 
                val res = DenseMatrix.zeros[Double](mrows, mcols)
                res(i % mrows, i / mrows) = 1.0
                res
            def appendTopAndBottomTo(top: Tuple, x: Any, bottom: Tuple) = 
                top ++ (x *: bottom)
            zerosWithIndex.map[[X] =>> Any]([U] => (t: U) => t match {
                case (x: DualNumberScalar[Double], y: Int) => 
                    val (top, bottom) = zeros.splitAt(y)
                    val dual = DualNumberScalar[Double](x.v, 1.0)
                    f(appendTopAndBottomTo(top.asInstanceOf[NonEmptyTuple].init, dual, bottom).asInstanceOf[T]).dv
                case (x: DualNumberColumnVector[Double], y: Int) => 
                    val (top, bottom) = zeros.splitAt(y)
                    val res = for (i <- 0 until x.v.length)
                        yield {
                            val dual = DualNumberColumnVector[Double](x.v, oneHotDenseVector(x.v.length, i))
                            f(appendTopAndBottomTo(top.asInstanceOf[NonEmptyTuple].init, dual, bottom).asInstanceOf[T]).dv
                        }
                    DenseVector(res.toArray)
                case (x: DualNumberRowVector[Double], y: Int) => 
                    val (top, bottom) = zeros.splitAt(y)
                    val res = for (i <- 0 until x.v.inner.length)
                        yield {
                            val dual = DualNumberRowVector[Double](x.v, oneHotDenseVector(x.v.inner.length, i).t)
                            f(appendTopAndBottomTo(top.asInstanceOf[NonEmptyTuple].init, dual, bottom).asInstanceOf[T]).dv
                        }
                    DenseVector(res.toArray).t
                case (x: DualNumberMatrix[Double], y: Int) =>
                    val (top, bottom) = zeros.splitAt(y)
                    val res = for {
                        i <- 0 until x.v.rows * x.v.cols
                    }
                        yield {
                            val dual = DualNumberMatrix[Double](x.v, oneHotDenseMatrix(x.v.rows, x.v.cols, i))
                            f(appendTopAndBottomTo(top.asInstanceOf[NonEmptyTuple].init, dual, bottom).asInstanceOf[T]).dv
                        }
                    new DenseMatrix(x.v.rows, x.v.cols, res.toArray)
            }).asInstanceOf[FromDuals[T]]
        forwardPlan(
            toZeros(t)
        )

    def f(ops: LinearAlgebraOps)(x: ops.Scalar, y: ops.ColumnVector): ops.Scalar = 
        import ops.*
        (x * y).sum

    def f2(ops: LinearAlgebraOps)(x: ops.Scalar, y: ops.Matrix): ops.Scalar =
        import ops.*
        (x * y).sum
        
    def f3(ops: LinearAlgebraOps)(
        x1: ops.Scalar,
        x2: ops.Scalar, 
        x3: ops.Scalar,
        x4: ops.Scalar,
        x5: ops.Scalar,
        x6: ops.Scalar,
        x7: ops.Scalar,
        x8: ops.Scalar,
        x9: ops.Scalar,
    ): ops.Scalar =
        import ops.*
        x1 * x2 * x3 * x4 * x5 * x6 * x7 * x8 * x9
        
    val fDual = f(BreezeVectorAlgebraForDualNumberDouble)
    val df = derive(fDual.tupled)

    val f2Dual = f2(BreezeVectorAlgebraForDualNumberDouble)
    val df2 = derive(f2Dual.tupled)

    val f3Dual = f3(BreezeVectorAlgebraForDualNumberDouble)
    val df3 = derive(f3Dual.tupled)

    println(df(1.0, DenseVector(1.0d, 2.0d)))
    println(df2(1.0, new DenseMatrix(2, 2, Array(1.0d, 2.0d, 1.0d, 2.0d))))
    println(df3(1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0))
package scalagrad.showcase.deeplearning

import scalagrad.linearalgebra.auto.reverse.delta.Deltas
import scalagrad.linearalgebra.auto.forward.dual.*

import breeze.linalg.*
import scalagrad.linearalgebra.auto.reverse.delta.DeltaScalar

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

    def liftScalar(x: Double) = DualNumberScalar(x, 0.0)
    def liftColumnVector(x: DenseVector[Double]) = DualNumberColumnVector(x, DenseVector.zeros[Double](x.length))
    def liftRowVector(x: Transpose[DenseVector[Double]]) = DualNumberRowVector(x, DenseVector.zeros[Double](x.inner.length).t)
    def liftMatrix(x: DenseMatrix[Double]) = DualNumberMatrix(x, DenseMatrix.zeros[Double](x.rows, x.cols))
        

    val scalar = DualNumberScalar[Double](1.0, 0.0)
    val rowVector = DualNumberRowVector[Double](DenseVector(1.0d, 2.0d).t, DenseVector.zeros[Double](2).t)

    def f(x: DualNumberScalar[Double], y: DualNumberColumnVector[Double]): DualNumberScalar[Double] = x
    def f2(x: DualNumberScalar[Double], y: DualNumberMatrix[Double]): DualNumberScalar[Double] = x

    type Values = Double | DenseVector[Double] | Transpose[DenseVector[Double]] | DenseMatrix[Double]

    def derive[T <: Tuple : DualTuple](f: T => DualNumberScalar[Double])(t: FromDuals[T]): FromDuals[T] = 
        def zipDelta(t: FromDuals[T]): T = 
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
                top match
                    case _: EmptyTuple => 
                        bottom match
                            case _: EmptyTuple => x
                            case bottom => x *: bottom
                    case top => 
                        bottom match
                            case _: EmptyTuple => top :* x
                            case bottom => top :* x :* bottom
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
        val zeros = zipDelta(t)
        val res = forwardPlan(zeros)
        res

    val df = derive(f.tupled)
    val df2 = derive(f2.tupled)

    println(df(1.0, DenseVector(1.0d, 2.0d)))
    println(df2(1.0, new DenseMatrix(2, 2, Array(1.0d, 2.0d, 1.0d, 2.0d))))
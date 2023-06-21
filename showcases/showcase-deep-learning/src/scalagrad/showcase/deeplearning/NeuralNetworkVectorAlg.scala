package scalagrad.showcase.deeplearning

import scala.io.Source
import scalagrad.showcase.deeplearning.Util.*
import scalagrad.api.linearalgebra.LinearAlgebraOps
import scalagrad.api.ScalaGrad
import scalagrad.linearalgebra.api.BreezeVectorAlgebraForDouble
import scalagrad.linearalgebra.auto.forward.DeriverBreezeForwardPlan
import scalagrad.linearalgebra.auto.forward.BreezeVectorAlgebraForDualNumberDouble
import scalagrad.linearalgebra.auto.forward.dual.{DualNumberMatrix, DualNumberColumnVector, DualNumberRowVector, DualNumberScalar}
import scalagrad.linearalgebra.auto.reverse.DeriverBreezeReversePlan
import scalagrad.linearalgebra.auto.reverse.DeriverBreezeReversePlan2
import scalagrad.linearalgebra.auto.reverse.BreezeVectorAlgebraForDualDeltaDouble
import scalagrad.linearalgebra.auto.reverse.BreezeVectorAlgebraForDualDeltaDoubleTotalOrder
import scalagrad.linearalgebra.auto.reverse.dual.{DeltaMonad, DualDeltaMatrix, DualDeltaColumnVector, DualDeltaRowVector, DualDeltaScalar}
import scalagrad.linearalgebra.auto.reverse.delta.{DeltaMatrix, DeltaColumnVector, DeltaRowVector, DeltaScalar}
import breeze.linalg.{DenseMatrix, DenseVector}

import scalagrad.spire.auto.dual.DualIsNumeric.given
import spire.math.Numeric
import spire.implicits.*

@main def neuralNetworkVectorAlg() = 

    val fishs = FishDataSet.load

    val xs = {
        fishs.map(fish => Vector(fish.length1, fish.length2, fish.length3, fish.height, fish.width)).toVector
    }
    val ys = fishs.map(_.weight).toVector

    var (xs_ss, _, _) = StandardScaler.scaleMatrix(xs)
    val (ys_ss, ys_mean, ys_std) = StandardScaler.scaleColumn(ys)

    def relu[P: Numeric](x: P): P = 
        val num = summon[Numeric[P]]
        if num.lt(x, num.zero) then num.zero else x

    def slowRelu[P: Numeric](x: P): P =
        var t = x 
        // do useless work to slow down the computation
        for (_ <- 0 until 7) {
            t += x 
            t -= x 
        }
        relu(t)

    def neuralNetwork(ops: LinearAlgebraOps)(
        x: ops.RowVector,
        firstW0: ops.ColumnVector, 
        firstWs: ops.Matrix,
        lastW0: ops.Scalar,
        lastWs: ops.ColumnVector,
    )(using num: Numeric[ops.Scalar]): ops.Scalar = 
        import ops.*
        val h = x * firstWs + firstW0.T
        val hh = h.map(relu)
        hh * lastWs + lastW0

    def square[P: Numeric](x: P) = 
        summon[Numeric[P]].times(x, x)

    def loss(ops: LinearAlgebraOps)(ys: ops.ColumnVector, ysHat: ops.ColumnVector)(using Numeric[ops.Scalar]): ops.Scalar =
        import ops.*
        val residuals = ys - ysHat
        val residualsSqr = residuals.map(square)
        residualsSqr.sum / (2 * ys.length).toScalar
        // residualsSqr.reduce([T] => (x: T, y: T) => (num: scala.math.Numeric[T]) ?=> num.plus(x, y)) / (2 * ys.length).toScalar

    def gradientDescent(ops: LinearAlgebraOps)(
        firstW0: ops.ColumnVector,
        firstWs: ops.Matrix,
        lastW0: ops.Scalar,
        lastWs: ops.ColumnVector,
        alpha: ops.Scalar, 
        n: Int
    )(
        dLoss: (ops.ColumnVector, ops.Matrix, ops.Scalar, ops.ColumnVector) => (ops.ColumnVector, ops.Matrix, ops.Scalar, ops.ColumnVector)
    ): (ops.ColumnVector, ops.Matrix, ops.Scalar, ops.ColumnVector) =
        if n == 0 then (firstW0, firstWs, lastW0, lastWs)
        else
            val (dFirstW0, dFirstWs, dLastW0, dLastWs) = dLoss(firstW0, firstWs, lastW0, lastWs)

            gradientDescent(ops)(
                firstW0 - alpha * dFirstW0,
                firstWs - alpha * dFirstWs,
                lastW0 - alpha * dLastW0,
                lastWs - alpha * dLastWs, 
                alpha,
                n - 1
            )(dLoss)

    val rand = scala.util.Random(42)
    
    import breeze.linalg.*

    val nFeatures = xs_ss(0).size
    val nHiddenUnits = 6
    val initFirstW0 = DenseVector.fill(nHiddenUnits)(rand.nextDouble())
    val initFirstWs = DenseMatrix.fill(nFeatures, nHiddenUnits)(rand.nextDouble())
    val initLastW0 = rand.nextDouble()
    val initLastWs = DenseVector.fill(nHiddenUnits)(rand.nextDouble())

    val xsSS = new DenseMatrix(xs_ss.head.length, xs_ss.length, xs_ss.flatten.toArray).t
    val ysSS = DenseVector(ys_ss.toArray)

    val initYsHat = StandardScaler.inverseScaleColumn( 
        (0 until xsSS.rows).map { i =>
            val row = xsSS(i, ::)
            neuralNetwork(BreezeVectorAlgebraForDouble)(
                row, initFirstW0, initFirstWs, initLastW0, initLastWs
            )
        }.toVector, 
        ys_mean, ys_std
    )

    println(f"${Math.sqrt(loss(BreezeVectorAlgebraForDouble)(DenseVector(ys.toArray), DenseVector(initYsHat.toArray)))}g  -- RMSE with initial weights")
    
    def lossF(ops: LinearAlgebraOps)(xs: ops.Matrix, ys: ops.ColumnVector)(
        firstW0: ops.ColumnVector,
        firstWs: ops.Matrix,
        lastW0: ops.Scalar,
        lastWs: ops.ColumnVector,
    )(using Numeric[ops.Scalar]): ops.Scalar =
        import ops.*
        val h = xs * firstWs + firstW0.T
        val hh = h.map(relu)
            // ops match 
            //     case breezeOps: BreezeVectorAlgebraForDualDeltaDouble.type =>
            //         // breezeOps.elementWiseOpsMForward( // derive relu with forward mode
            //         //     h.asInstanceOf[DualDeltaMatrix[Double]], 
            //         //     slowRelu
            //         // ).asInstanceOf[ops.Matrix]
            //         h.map(slowRelu) // derive relu with reverse mode
            //     case _ =>
            //         h.map(relu)
        val ysHat = hh * lastWs + lastW0
        loss(ops)(ys, ysHat)
    
    time {
        val iters = 1
        println(f"Start forward-mode with ${iters} iterations")
        val gradientDescentF = gradientDescent(BreezeVectorAlgebraForDouble)(
            initFirstW0, initFirstWs, initLastW0, initLastWs, 0.01, iters // 100_000
        )
        
        import DeriverBreezeForwardPlan.given
        val dLoss = ScalaGrad.derive(lossF(BreezeVectorAlgebraForDualNumberDouble)(
            DualNumberMatrix(xsSS, DenseMatrix.zeros[Double](xsSS.rows, xsSS.cols)),
            DualNumberColumnVector(ysSS, DenseVector.zeros[Double](ysSS.length)
        )))
        val (firstW0, firstWs, lastW0, lastWs) = gradientDescentF(dLoss)

        val ysHat = StandardScaler.inverseScaleColumn(
            (0 until xsSS.rows).map { i =>
                val row = xsSS(i, ::)
                neuralNetwork(BreezeVectorAlgebraForDouble)(
                    row, firstW0, firstWs, lastW0, lastWs
                )
            }.toVector, 
            ys_mean, ys_std
        )

        println(f"${Math.sqrt(loss(BreezeVectorAlgebraForDouble)(DenseVector(ys.toArray), DenseVector(ysHat.toArray)))}g  -- RMSE with learned weights (${iters} iterations)")
    }
    time {
        val iters = 1
        println(f"Start reverse-mode with ${iters} iterations")
        val gradientDescentF = gradientDescent(BreezeVectorAlgebraForDouble)(
            initFirstW0, initFirstWs, initLastW0, initLastWs, 0.01, iters // 100_000
        )
        
        val lalalala = DeriverBreezeReversePlan2
        import lalalala.given
        val op2 = BreezeVectorAlgebraForDualDeltaDoubleTotalOrder()
        val dLoss = ScalaGrad.derive(lossF(op2)(
            op2.liftMatrix(xsSS),
            op2.liftColumnVector(ysSS)
        ))
        val (firstW0, firstWs, lastW0, lastWs) = gradientDescentF(dLoss)

        val ysHat = StandardScaler.inverseScaleColumn( 
            (0 until xsSS.rows).map { i =>
                val row = xsSS(i, ::)
                neuralNetwork(BreezeVectorAlgebraForDouble)(
                    row, firstW0, firstWs, lastW0, lastWs
                )
            }.toVector, 
            ys_mean, ys_std
        )

        println(f"${Math.sqrt(loss(BreezeVectorAlgebraForDouble)(DenseVector(ys.toArray), DenseVector(ysHat.toArray)))}g  -- RMSE with learned weights (${iters} iterations)")
    }
    /*
    time {
        val iters = 1000
        println(f"Start reverse-mode with ${iters} iterations")
        val gradientDescentF = gradientDescent(BreezeVectorAlgebraForDouble)(
            initFirstW0, initFirstWs, initLastW0, initLastWs, 0.01, iters // 100_000
        )
        
        import DeriverBreezeReversePlan.given
        val dLoss = ScalaGrad.derive(lossF(BreezeVectorAlgebraForDualDeltaDouble)(
            DualDeltaMatrix(xsSS, DeltaMatrix.zero),
            DualDeltaColumnVector(ysSS, DeltaColumnVector.zero)
        ))
        val (firstW0, firstWs, lastW0, lastWs) = gradientDescentF(dLoss)

        val ysHat = StandardScaler.inverseScaleColumn( 
            (0 until xsSS.rows).map { i =>
                val row = xsSS(i, ::)
                neuralNetwork(BreezeVectorAlgebraForDouble)(
                    row, firstW0, firstWs, lastW0, lastWs
                )
            }.toVector, 
            ys_mean, ys_std
        )

        println(f"${Math.sqrt(loss(BreezeVectorAlgebraForDouble)(DenseVector(ys.toArray), DenseVector(ysHat.toArray)))}g  -- RMSE with learned weights (${iters} iterations)")
    }*/
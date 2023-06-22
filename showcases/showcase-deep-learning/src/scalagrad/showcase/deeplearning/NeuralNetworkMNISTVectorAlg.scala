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
import scalagrad.linearalgebra.auto.reverse.DeriverBreezeReversePlanMonad
import scalagrad.linearalgebra.auto.reverse.DeriverBreezeReversePlan2
import scalagrad.linearalgebra.auto.reverse.BreezeVectorAlgebraForDualDeltaDouble
import scalagrad.linearalgebra.auto.reverse.BreezeVectorAlgebraForDualDeltaDoubleMonad
import scalagrad.linearalgebra.auto.reverse.BreezeVectorAlgebraForDualDeltaDoubleTotalOrder
import scalagrad.linearalgebra.auto.reverse.dual.{DualDeltaMatrix, DualDeltaColumnVector, DualDeltaRowVector, DualDeltaScalar}
import scalagrad.linearalgebra.auto.reverse.delta.{DeltaMatrix, DeltaColumnVector, DeltaRowVector, DeltaScalar}
import scalagrad.linearalgebra.auto.reverse.dualMonad.*
import breeze.linalg.{DenseMatrix, DenseVector}

import scalagrad.spire.auto.dual.DualIsNumeric.given
import spire.math.Numeric
import spire.algebra.Trig
import spire.implicits.*
import scalagrad.showcase.deeplearning.MNISTDataSet.MNISTEntry

def normalize(pixel: Double) = pixel / 255

def preprocess(data: Iterator[MNISTEntry], batchSize: Int) = 
    val dataL = LazyList.from(data)
    val xs = dataL.map(entry => entry.pixels.map(normalize).toVector)
    val ys = dataL.map(_.label)

    val xsBatches = LazyList.from(
        xs.grouped(batchSize)
            .map(xBatch => new DenseMatrix(xBatch.head.length, xBatch.length, xBatch.flatten.toArray).t)
    )
    val ysBatches = LazyList.from(
        ys.grouped(batchSize)
            .map(yBatch => DenseVector(yBatch.toArray))
            .map(yBatch => {
                DenseMatrix.tabulate(yBatch.length, MNISTDataSet.nLabels) { (i, j) =>
                    if yBatch(i) == j then 1.0 else 0.0
                }
            })
    )

    (xsBatches, ysBatches)

def relu[P: Numeric](x: P): P = 
    val num = summon[Numeric[P]]
    if num.lt(x, num.zero) then num.zero else x

def dSoftmax(x: DenseVector[Double]): DenseMatrix[Double] =
    val tt = softmax(BreezeVectorAlgebraForDouble)(x)
    val softmaxR = softmax(BreezeVectorAlgebraForDouble)(x).toArray
    DenseMatrix.tabulate(softmaxR.length, softmaxR.length) { (i, j) =>
        if i == j then softmaxR(i) * (1 - softmaxR(i)) else -softmaxR(i) * softmaxR(j)
    }

def dStableSoftmax(x: DenseVector[Double]): DenseMatrix[Double] =
    val maxElement = breeze.linalg.max(x)
    dSoftmax(x - maxElement)

def softmax(ops: LinearAlgebraOps)(x: ops.ColumnVector)(using t: Trig[ops.Scalar]): ops.ColumnVector = 
    import ops.*
    val exps = x.map(x => t.exp(x))
    val sumExps = exps.sum
    ops.divideCVS(exps, sumExps)

def stableSoftmax(ops: LinearAlgebraOps)(x: ops.ColumnVector)(using t: Trig[ops.Scalar]): ops.ColumnVector = 
    val maxElement = x.elements.maxBy(_.toDouble)
    softmax(ops)(x - maxElement)

def neuralNetwork(ops: LinearAlgebraOps)(
    xs: ops.Matrix,
    firstW0: ops.ColumnVector, 
    firstWs: ops.Matrix,
    lastW0: ops.ColumnVector,
    lastWs: ops.Matrix,
)(using num: Numeric[ops.Scalar], trig: Trig[ops.Scalar]): ops.Matrix = 
    import ops.*
    val h = xs * firstWs + firstW0.T
    val hh = h.map(relu)
    val o = hh * lastWs + lastW0.T
    o.mapRows(row => stableSoftmax(ops)(row.T).T)

def neuralNetwork2(ops: LinearAlgebraOps)(
    xs: ops.Matrix,
    firstW0: ops.ColumnVector, 
    firstWs: ops.Matrix,
    lastW0: ops.ColumnVector,
    lastWs: ops.Matrix,
)(using num: Numeric[ops.Scalar], trig: Trig[ops.Scalar]): ops.Matrix = 
    import ops.*
    val h = xs * firstWs + firstW0.T
    val hh = h.map(relu)
    val o = hh * lastWs + lastW0.T
    o.mapRows2(row => stableSoftmax(ops)(row.T).T)

def neuralNetwork2a(ops: LinearAlgebraOps)(
    xs: ops.Matrix,
    firstW0: ops.ColumnVector, 
    firstWs: ops.Matrix,
    lastW0: ops.ColumnVector,
    lastWs: ops.Matrix,
)(using num: Numeric[ops.Scalar], trig: Trig[ops.Scalar]): ops.Matrix = 
    import ops.*
    val h = xs * firstWs + firstW0.T
    val hh = h.map2(relu)
    val o = hh * lastWs + lastW0.T
    o.mapRows2(row => stableSoftmax(ops)(row.T).T)

def neuralNetwork3(ops: LinearAlgebraOps)(
    xs: ops.Matrix,
    firstW0: ops.ColumnVector, 
    firstWs: ops.Matrix,
    lastW0: ops.ColumnVector,
    lastWs: ops.Matrix,
)(using num: Numeric[ops.Scalar], trig: Trig[ops.Scalar]): ops.Matrix = 
    import ops.*
    val h = xs * firstWs + firstW0.T
    val hh = ops match 
        case iOps: BreezeVectorAlgebraForDualDeltaDouble.type => iOps.elementWiseOpsMForward(h.asInstanceOf[iOps.Matrix], relu).asInstanceOf[ops.Matrix]
        case iOps: BreezeVectorAlgebraForDualDeltaDoubleTotalOrder=> iOps.elementWiseOpsMForward(h.asInstanceOf[iOps.Matrix], relu).asInstanceOf[ops.Matrix]
        case _ => h.map(relu)
    val o = hh * lastWs + lastW0.T
    // TODO use forward mode here
    def stableSoftmaxRV(ops: LinearAlgebraOps)(x: ops.RowVector)(using t: Trig[ops.Scalar]): ops.RowVector = 
        stableSoftmax(ops)(x.T).T
    ops match
        case iOps: BreezeVectorAlgebraForDualDeltaDouble.type => iOps.rowWiseOpsMForward(o.asInstanceOf[iOps.Matrix], stableSoftmaxRV(BreezeVectorAlgebraForDualNumberDouble)).asInstanceOf[ops.Matrix]
        case iOps: BreezeVectorAlgebraForDualDeltaDoubleTotalOrder=> iOps.rowWiseOpsMForward(o.asInstanceOf[iOps.Matrix], stableSoftmaxRV(BreezeVectorAlgebraForDualNumberDouble)).asInstanceOf[ops.Matrix]
        case _ => o.mapRows(row => stableSoftmaxRV(ops)(row))

def clip(ops: LinearAlgebraOps)(x: ops.Scalar)(using n: Numeric[ops.Scalar]): ops.Scalar =
    val epsilon: Double = 1e-07
    val minS = ops.liftToScalar(epsilon)
    val maxS = ops.liftToScalar(1.0 - epsilon)
    if n.lt(x, minS) then minS
    else if n.gt(x, maxS) then maxS
    else x

def crossEntropy(ops: LinearAlgebraOps)(ys: ops.Matrix, ysHat: ops.Matrix)(using n: Numeric[ops.Scalar], t: Trig[ops.Scalar]): ops.Scalar =
    import ops.*
    val logYsHat = ysHat.map(clip(ops)).map(t.log)
    val logYsHatYs = logYsHat *:* ys
    ops.liftToScalar(-1) * ops.divideSS(logYsHatYs.sum, ops.liftToScalar(logYsHat.rows))

def crossEntropy2a(ops: LinearAlgebraOps)(ys: ops.Matrix, ysHat: ops.Matrix)(using n: Numeric[ops.Scalar], t: Trig[ops.Scalar]): ops.Scalar =
    import ops.*
    val logYsHat = ysHat.map2(clip(ops)).map2(t.log)
    val logYsHatYs = logYsHat *:* ys
    ops.liftToScalar(-1) * ops.divideSS(logYsHatYs.sum, ops.liftToScalar(logYsHat.rows))

def crossEntropy3(ops: LinearAlgebraOps)(ys: ops.Matrix, ysHat: ops.Matrix)(using n: Numeric[ops.Scalar], t: Trig[ops.Scalar]): ops.Scalar =
    import ops.*
        val logYsHat = ops match 
        case iOps: BreezeVectorAlgebraForDualDeltaDouble.type => 
            iOps.elementWiseOpsMForward(
                iOps.elementWiseOpsMForward(ysHat.asInstanceOf[iOps.Matrix], clip(BreezeVectorAlgebraForDualNumberDouble)),
                summon[Trig[BreezeVectorAlgebraForDualNumberDouble.Scalar]].log
            ).asInstanceOf[ops.Matrix]
        case iOps: BreezeVectorAlgebraForDualDeltaDoubleTotalOrder=> 
            iOps.elementWiseOpsMForward(
                iOps.elementWiseOpsMForward(ysHat.asInstanceOf[iOps.Matrix], clip(BreezeVectorAlgebraForDualNumberDouble)),
                summon[Trig[BreezeVectorAlgebraForDualNumberDouble.Scalar]].log
            ).asInstanceOf[ops.Matrix]
        case _ => ysHat.map(clip(ops)).map(t.log)
    val logYsHatYs = logYsHat *:* ys
    ops.liftToScalar(-1) * ops.divideSS(logYsHatYs.sum, ops.liftToScalar(logYsHat.rows))

def cycle[T](seq: Seq[T]): LazyList[T] = {
    def inner(s: Seq[T]): LazyList[T] = s match {
        case head +: tail => head #:: inner(tail)
        case _            => inner(seq)
    }
    inner(seq)
}

val breezeOps = BreezeVectorAlgebraForDouble
def miniBatchGradientDescent
(data: LazyList[(breezeOps.Matrix, breezeOps.Matrix)])
(
    firstW0: breezeOps.ColumnVector,
    firstWs: breezeOps.Matrix,
    lastW0: breezeOps.ColumnVector,
    lastWs: breezeOps.Matrix,
    alpha: breezeOps.Scalar, 
    n: Int,
    verbose: Boolean = false
)(
    createDLoss: (breezeOps.Matrix, breezeOps.Matrix) => (breezeOps.ColumnVector, breezeOps.Matrix, breezeOps.ColumnVector, breezeOps.Matrix) => (breezeOps.ColumnVector, breezeOps.Matrix, breezeOps.ColumnVector, breezeOps.Matrix)
)(using Numeric[breezeOps.Scalar], Trig[breezeOps.Scalar]): (breezeOps.ColumnVector, breezeOps.Matrix, breezeOps.ColumnVector, breezeOps.Matrix) =
    if n == 0 then (firstW0, firstWs, lastW0, lastWs)
    else
        if (verbose && n % 100 == 0) println("Running iteration " + n)
        val (xsBatch, ysBatch) = data.head
        val dLoss = createDLoss(xsBatch, ysBatch)
        val (dFirstW0, dFirstWs, dLastW0, dLastWs) = dLoss(firstW0, firstWs, lastW0, lastWs)
        
        import breezeOps.*
        miniBatchGradientDescent(data.tail)(
            firstW0 - alpha * dFirstW0,
            firstWs - alpha * dFirstWs,
            lastW0 - alpha * dLastW0,
            lastWs - alpha * dLastWs, 
            alpha,
            n - 1,
            verbose
        )(createDLoss)

def gradientDescent(ops: LinearAlgebraOps)(
    firstW0: ops.ColumnVector,
    firstWs: ops.Matrix,
    lastW0: ops.ColumnVector,
    lastWs: ops.Matrix,
    alpha: ops.Scalar, 
    n: Int
)(
    dLoss: (ops.ColumnVector, ops.Matrix, ops.ColumnVector, ops.Matrix) => (ops.ColumnVector, ops.Matrix, ops.ColumnVector, ops.Matrix)
): (ops.ColumnVector, ops.Matrix, ops.ColumnVector, ops.Matrix) =
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

def lossF(ops: LinearAlgebraOps)(xs: ops.Matrix, ys: ops.Matrix)(
    firstW0: ops.ColumnVector,
    firstWs: ops.Matrix,
    lastW0: ops.ColumnVector,
    lastWs: ops.Matrix,
)(using Numeric[ops.Scalar], Trig[ops.Scalar]): ops.Scalar =
    val ysHat = neuralNetwork(ops)(xs, firstW0, firstWs, lastW0, lastWs)
    crossEntropy(ops)(ys, ysHat)

def lossF2(ops: LinearAlgebraOps)(xs: ops.Matrix, ys: ops.Matrix)(
    firstW0: ops.ColumnVector,
    firstWs: ops.Matrix,
    lastW0: ops.ColumnVector,
    lastWs: ops.Matrix,
)(using Numeric[ops.Scalar], Trig[ops.Scalar]): ops.Scalar =
    val ysHat = neuralNetwork2(ops)(xs, firstW0, firstWs, lastW0, lastWs)
    crossEntropy(ops)(ys, ysHat)

def lossF2a(ops: LinearAlgebraOps)(xs: ops.Matrix, ys: ops.Matrix)(
    firstW0: ops.ColumnVector,
    firstWs: ops.Matrix,
    lastW0: ops.ColumnVector,
    lastWs: ops.Matrix,
)(using Numeric[ops.Scalar], Trig[ops.Scalar]): ops.Scalar =
    val ysHat = neuralNetwork2a(ops)(xs, firstW0, firstWs, lastW0, lastWs)
    crossEntropy2a(ops)(ys, ysHat)

def lossF3(ops: LinearAlgebraOps)(xs: ops.Matrix, ys: ops.Matrix)(
    firstW0: ops.ColumnVector,
    firstWs: ops.Matrix,
    lastW0: ops.ColumnVector,
    lastWs: ops.Matrix,
)(using Numeric[ops.Scalar], Trig[ops.Scalar]): ops.Scalar =
    val ysHat = neuralNetwork3(ops)(xs, firstW0, firstWs, lastW0, lastWs)
    crossEntropy3(ops)(ys, ysHat)

def accuracy(yHatProp: DenseMatrix[Double], yM: DenseMatrix[Double]): Double =
    import breeze.linalg.{Vector => BreezeVector, *}
    val yHat = yHatProp(*, ::).map(x => argmax(x))
    val y = yM(*, ::).map(x => argmax(x))
    val correct = yHat.toArray.zip(y.toArray).map((yHat, y) => if yHat == y then 1 else 0).sum
    correct.toDouble / yHat.length

def makePredictions(xs: DenseMatrix[Double])(
    firstW0: DenseVector[Double],
    firstWs: DenseMatrix[Double],
    lastW0: DenseVector[Double],
    lastWs: DenseMatrix[Double],
): DenseMatrix[Double] =
    neuralNetwork(BreezeVectorAlgebraForDouble)(
        xs, firstW0, firstWs, lastW0, lastWs
    )

def getRandomWeights(nFeatures: Int, nHiddenUnits: Int, nOutputUnits: Int): (
    DenseVector[Double],
    DenseMatrix[Double],
    DenseVector[Double],
    DenseMatrix[Double],
) = 
    val rand = scala.util.Random(42)
    (
        DenseVector.fill(nHiddenUnits)(rand.nextDouble() - 0.5),
        DenseMatrix.fill(nFeatures, nHiddenUnits)(rand.nextDouble() - 0.5),
        DenseVector.fill(nOutputUnits)(rand.nextDouble() - 0.5),
        DenseMatrix.fill(nHiddenUnits, nOutputUnits)(rand.nextDouble() - 0.5),
    )

@main def neuralNetworkMNISTVectorAlg() = 

    // TODO
    // Current implementation has accuracy of 80% on test set after 1 epoch of training
    // equivalent tensorflow implementation is around 40% accuracy... why?
    // With batch_size == 1 accuracy is 75% in tensorflow...

    // forward pass only
    // 32 => 4s (10 epochs)

    // 32 (tensorflow) =>  1s
    //  1 (tensorflow) => 23s 
    // Before fix
    // 32 => 480s       => Without useless caches: 108s
    // 16 => 347s
    //  8 => 332s
    //  4 => 328s
    //  1 => 290s
    // After fix
    // 256 => 11s       => Without useless caches: 11s     => With forward softmax: 8s
    // 128 => 12s
    //  64 => 12s
    //  32 => 16s       => Without useless caches: 12s
    //  16 => 17s
    //   8 => 21s
    //   4 => 25s
    //   1 => 59s       => Without useless caches: 15s

    val batchSize = 8
    val nFeatures = MNISTDataSet.nFeatures
    val nHiddenUnits = 36
    val nOutputUnits = MNISTDataSet.nLabels

    val (xsTrain, ysTrain) = preprocess(MNISTDataSet.loadTrain, batchSize)
    val (xsTest, ysTest) = preprocess(MNISTDataSet.loadTest, batchSize)

    val rand = scala.util.Random(42)
    
    val (initFirstW0, initFirstWs, initLastW0, initLastWs) = getRandomWeights(nFeatures, nHiddenUnits, nOutputUnits)

    val eagerData = cycle(xsTrain.zip(ysTrain).toList)

    val (xsM, ysM) = eagerData.head
    /*
    time {
        // val epochs = 10
        // val iters = epochs * MNISTDataSet.trainSize / batchSize
        val iters = 1
        println(f"Start forward-pass only on single batch with ${iters} iterations")
        
        val initYsHatM = makePredictions(xsM)(initFirstW0, initFirstWs, initLastW0, initLastWs)
        for (i <- 0 until iters - 1) {
            makePredictions(xsM)(initFirstW0, initFirstWs, initLastW0, initLastWs)
        }

        println(initYsHatM.rows + " " + initYsHatM.cols)
        println(ysM.rows + " " + ysM.cols)

        println(f"${crossEntropy(BreezeVectorAlgebraForDouble)(ysM, initYsHatM)}  -- with initial weights")
        println(f"${lossF(BreezeVectorAlgebraForDouble)(xsM, ysM)(initFirstW0, initFirstWs, initLastW0, initLastWs)}  -- with initial weights")
        println("train acc: " + accuracy(initYsHatM, ysM) * 100 + " -- with initial weights")
        
    }*/
    time {
        val iters = 1
        println(f"Start forward-mode on single batch with ${iters} iterations")
        val gradientDescentF = gradientDescent(BreezeVectorAlgebraForDouble)(
            initFirstW0, initFirstWs, initLastW0, initLastWs, 0.01, iters
        )
        
        import DeriverBreezeForwardPlan.given
        val dLoss = ScalaGrad.derive(lossF(BreezeVectorAlgebraForDualNumberDouble)(
            DualNumberMatrix(xsM, DenseMatrix.zeros[Double](xsM.rows, xsM.cols)),
            DualNumberMatrix(ysM, DenseMatrix.zeros[Double](ysM.rows, ysM.cols))
        ))
        val (firstW0, firstWs, lastW0, lastWs) = gradientDescentF(dLoss)

        val ysHatM = makePredictions(xsM)(firstW0, firstWs, lastW0, lastWs)

        println("train acc: " + accuracy(ysHatM, ysM) * 100)
        println(f"${crossEntropy(BreezeVectorAlgebraForDouble)(ysM, ysHatM)}  -- with learned weights (${iters} iterations)")
    }
    /*
    time {
        val iters = 1
        println(f"Start reverse-mode total-order on single batch with ${iters} iterations")
        val gradientDescentF = gradientDescent(BreezeVectorAlgebraForDouble)(
            initFirstW0, initFirstWs, initLastW0, initLastWs, 0.01, iters
        )
        
        val plan = DeriverBreezeReversePlan2
        val ops = plan.createOps()
        import plan.given
        val dLoss = ScalaGrad.derive(lossF2(ops)(
            ops.liftMatrix(xsM),
            ops.liftMatrix(ysM)
        ))
        val (firstW0, firstWs, lastW0, lastWs) = gradientDescentF(dLoss)

        val ysHatM = makePredictions(xsM)(firstW0, firstWs, lastW0, lastWs)

        println("train acc: " + accuracy(ysHatM, ysM) * 100)
        println(f"${crossEntropy(BreezeVectorAlgebraForDouble)(ysM, ysHatM)}  -- with learned weights (${iters} iterations)")
    }
    time {
        val iters = 1 // 1000 / batchSize
        println(f"Start reverse-mode on single batch with ${iters} iterations")
        val gradientDescentF = gradientDescent(BreezeVectorAlgebraForDouble)(
            initFirstW0, initFirstWs, initLastW0, initLastWs, 0.01, iters
        )
        
        import DeriverBreezeReversePlan.given
        val dLoss = ScalaGrad.derive(lossF2(BreezeVectorAlgebraForDualDeltaDouble)(
            BreezeVectorAlgebraForDualDeltaDouble.liftMatrix(xsM),
            BreezeVectorAlgebraForDualDeltaDouble.liftMatrix(ysM)
        ))
        val (firstW0, firstWs, lastW0, lastWs) = gradientDescentF(dLoss)

        val ysHatM = makePredictions(xsM)(firstW0, firstWs, lastW0, lastWs)

        println("train acc: " + accuracy(ysHatM, ysM) * 100)
        println(f"${crossEntropy(BreezeVectorAlgebraForDouble)(ysM, ysHatM)}  -- with learned weights (${iters} iterations)")
    }
    time {
        val iters = 1 // 1000 / batchSize
        println(f"Start reverse-mode monad on single batch with ${iters} iterations")
        val gradientDescentF = gradientDescent(BreezeVectorAlgebraForDouble)(
            initFirstW0, initFirstWs, initLastW0, initLastWs, 0.01, iters
        )
        
        import DeriverBreezeReversePlanMonad.given
        val dLoss = ScalaGrad.derive(lossF2(BreezeVectorAlgebraForDualDeltaDoubleMonad)(
            BreezeVectorAlgebraForDualDeltaDoubleMonad.liftMatrix(xsM),
            BreezeVectorAlgebraForDualDeltaDoubleMonad.liftMatrix(ysM)
        ))
        val (firstW0, firstWs, lastW0, lastWs) = gradientDescentF(dLoss)

        val ysHatM = makePredictions(xsM)(firstW0, firstWs, lastW0, lastWs)

        println("train acc: " + accuracy(ysHatM, ysM) * 100)
        println(f"${crossEntropy(BreezeVectorAlgebraForDouble)(ysM, ysHatM)}  -- with learned weights (${iters} iterations)")
    }*/

@main def neuralNetworkMNISTVectorAlgPerformance() = 
    case class PerformanceTest(
        mode: String,
        batchSize: Int,
        samples: Int,
    )

    object PerformanceTest:
        def create(mode: String, batchSizes: List[Int], samples: Int): List[PerformanceTest] =
            batchSizes.map(batchSize => PerformanceTest(mode, batchSize, samples))

    val nFeatures = MNISTDataSet.nFeatures
    val nHiddenUnits = 36
    val nOutputUnits = MNISTDataSet.nLabels

    val (initFirstW0, initFirstWs, initLastW0, initLastWs) = getRandomWeights(nFeatures, nHiddenUnits, nOutputUnits)

    println("****** START PERFORMANCE TESTS ******")
    val (xsTest, ysTest) = preprocess(MNISTDataSet.loadTest, 32)
    val batchSizesReverseMode = List(64, 128)
    // val batchSizesReverseMode = List(8, 16, 32, 64, 128, 256, 512)
    val batchSizesForwardMode = List(1, 2, 4, 8)
    // val batchSizesForwardMode = List(1, 2, 4, 8)
    
    for {
        test <-
            PerformanceTest.create("reverseM", batchSizesReverseMode, 60_000)
            ++ PerformanceTest.create("reverseM2", batchSizesReverseMode, 1_000)
            ++ PerformanceTest.create("forward", batchSizesForwardMode, 1)
            ++ PerformanceTest.create("reverse", batchSizesReverseMode, 60_000)
            ++ PerformanceTest.create("reverse-to", batchSizesReverseMode, 60_000)
            ++ PerformanceTest.create("forward2", batchSizesForwardMode, 1)
            ++ PerformanceTest.create("reverse2", batchSizesReverseMode, 1_000)
            ++ PerformanceTest.create("reverse-to2", batchSizesReverseMode, 60_000)
            ++ PerformanceTest.create("reverse-to2a", batchSizesReverseMode, 60_000)
            ++ PerformanceTest.create("reverse3", batchSizesReverseMode, 60_000)
            ++ PerformanceTest.create("reverse-to3", batchSizesReverseMode, 60_000)
    } {
            val (mode, batchSize, samples) = (test.mode, test.batchSize, test.samples)
            val (xsTrain, ysTrain) = preprocess(MNISTDataSet.loadTrain, batchSize)
            val eagerData = cycle(xsTrain.zip(ysTrain).toList)

            val iters: Int = Math.max(samples / batchSize, 1)

            val miniBatchGradientDescentF = miniBatchGradientDescent
                (eagerData)
                (initFirstW0, initFirstWs, initLastW0, initLastWs, 0.01, iters)

            val createDLoss = mode match {
                case "forward" => {
                    import DeriverBreezeForwardPlan.given
                    def createDLoss(xs: DenseMatrix[Double], ys: DenseMatrix[Double]) =
                        ScalaGrad.derive(lossF(BreezeVectorAlgebraForDualNumberDouble)(
                            BreezeVectorAlgebraForDualNumberDouble.liftMatrix(xs),
                            BreezeVectorAlgebraForDualNumberDouble.liftMatrix(ys)
                        ))
                    createDLoss
                }
                case "forward2" => {
                    import DeriverBreezeForwardPlan.given
                    def createDLoss(xs: DenseMatrix[Double], ys: DenseMatrix[Double]) =
                        ScalaGrad.derive(lossF2(BreezeVectorAlgebraForDualNumberDouble)(
                            BreezeVectorAlgebraForDualNumberDouble.liftMatrix(xs),
                            BreezeVectorAlgebraForDualNumberDouble.liftMatrix(ys)
                        ))
                    createDLoss
                }
                case "reverse" => {
                    import DeriverBreezeReversePlan.given
                    def createDLoss(xs: DenseMatrix[Double], ys: DenseMatrix[Double]) =
                        ScalaGrad.derive(lossF(BreezeVectorAlgebraForDualDeltaDouble)(
                            BreezeVectorAlgebraForDualDeltaDouble.liftMatrix(xs),
                            BreezeVectorAlgebraForDualDeltaDouble.liftMatrix(ys)
                        ))
                    createDLoss
                }
                case "reverse2" => {
                    import DeriverBreezeReversePlan.given
                    def createDLoss(xs: DenseMatrix[Double], ys: DenseMatrix[Double]) =
                        ScalaGrad.derive(lossF2(BreezeVectorAlgebraForDualDeltaDouble)(
                            BreezeVectorAlgebraForDualDeltaDouble.liftMatrix(xs),
                            BreezeVectorAlgebraForDualDeltaDouble.liftMatrix(ys)
                        ))
                    createDLoss
                }
                case "reverse3" => {
                    import DeriverBreezeReversePlan.given
                    def createDLoss(xs: DenseMatrix[Double], ys: DenseMatrix[Double]) =
                        ScalaGrad.derive(lossF3(BreezeVectorAlgebraForDualDeltaDouble)(
                            BreezeVectorAlgebraForDualDeltaDouble.liftMatrix(xs),
                            BreezeVectorAlgebraForDualDeltaDouble.liftMatrix(ys)
                        ))
                    createDLoss
                }
                case "reverseM" => {
                    import DeriverBreezeReversePlanMonad.given
                    def createDLoss(xs: DenseMatrix[Double], ys: DenseMatrix[Double]) =
                        ScalaGrad.derive(lossF(BreezeVectorAlgebraForDualDeltaDoubleMonad)(
                            BreezeVectorAlgebraForDualDeltaDoubleMonad.liftMatrix(xs),
                            BreezeVectorAlgebraForDualDeltaDoubleMonad.liftMatrix(ys)
                        ))
                    createDLoss
                }
                case "reverseM2" => {
                    import DeriverBreezeReversePlanMonad.given
                    def createDLoss(xs: DenseMatrix[Double], ys: DenseMatrix[Double]) =
                        ScalaGrad.derive(lossF2(BreezeVectorAlgebraForDualDeltaDoubleMonad)(
                            BreezeVectorAlgebraForDualDeltaDoubleMonad.liftMatrix(xs),
                            BreezeVectorAlgebraForDualDeltaDoubleMonad.liftMatrix(ys)
                        ))
                    createDLoss
                }
                case "reverse-to" => {
                    import DeriverBreezeReversePlan2.given
                    val ops = BreezeVectorAlgebraForDualDeltaDoubleTotalOrder()
                    def createDLoss(xs: DenseMatrix[Double], ys: DenseMatrix[Double]) =
                        ScalaGrad.derive(lossF(ops)(
                            ops.liftMatrix(xs),
                            ops.liftMatrix(ys)
                        ))
                    createDLoss
                }
                case "reverse-to2" => {
                    import DeriverBreezeReversePlan2.given
                    val ops = BreezeVectorAlgebraForDualDeltaDoubleTotalOrder()
                    def createDLoss(xs: DenseMatrix[Double], ys: DenseMatrix[Double]) =
                        ScalaGrad.derive(lossF2(ops)(
                            ops.liftMatrix(xs),
                            ops.liftMatrix(ys)
                        ))
                    createDLoss
                }
                case "reverse-to2a" => {
                    import DeriverBreezeReversePlan2.given
                    val ops = BreezeVectorAlgebraForDualDeltaDoubleTotalOrder()
                    def createDLoss(xs: DenseMatrix[Double], ys: DenseMatrix[Double]) =
                        ScalaGrad.derive(lossF2a(ops)(
                            ops.liftMatrix(xs),
                            ops.liftMatrix(ys)
                        ))
                    createDLoss
                }
                case "reverse-to3" => {
                    import DeriverBreezeReversePlan2.given
                    val ops = BreezeVectorAlgebraForDualDeltaDoubleTotalOrder()
                    def createDLoss(xs: DenseMatrix[Double], ys: DenseMatrix[Double]) =
                        ScalaGrad.derive(lossF3(ops)(
                            ops.liftMatrix(xs),
                            ops.liftMatrix(ys)
                        ))
                    createDLoss
                }
            }

            val ((firstW0, firstWs, lastW0, lastWs), trainTime) = timeMeasure({
                miniBatchGradientDescentF(createDLoss)
            }, unit = java.util.concurrent.TimeUnit.MILLISECONDS)
            val ysHatTest = xsTest.map(xs => makePredictions(xs)(firstW0, firstWs, lastW0, lastWs))
            val accuracyTestBatch = ysHatTest.zip(ysTest).map((ysHat, ys) => accuracy(ysHat, ys)).toList
            val accuracyTest = accuracyTestBatch.sum / accuracyTestBatch.length
            val lossTestBatch = ysHatTest.zip(ysTest).map((ysHat, ys) => crossEntropy(BreezeVectorAlgebraForDouble)(ys, ysHat)).toList
            val lossTest = lossTestBatch.sum / lossTestBatch.length

            val trainTimeNormalized = trainTime * (60_000d / iters / batchSize) 

            println(
                List(
                    f"mode=$mode%15s",
                    f"batch_size=${batchSize}%3d",
                    f"iters=${iters}%5d",
                    f"testLoss=${lossTest}%.1f",
                    f"testAcc=${accuracyTest * 100}%3f",
                    f"trainTime=${trainTime / 1000}%3.1fs",
                    f"trainTime/60_000≈${trainTimeNormalized / 1000}%10.2fs",
                ).mkString("\t")
            )
            Thread.sleep(5000) // to avoid overheating and give GC some time to clean up memory
    } 
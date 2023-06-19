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
import scalagrad.linearalgebra.auto.reverse.BreezeVectorAlgebraForDualDeltaDouble
import scalagrad.linearalgebra.auto.reverse.dual.{DeltaMonad, DualDeltaMatrix, DualDeltaColumnVector, DualDeltaRowVector, DualDeltaScalar}
import scalagrad.linearalgebra.auto.reverse.delta.{DeltaMatrix, DeltaColumnVector, DeltaRowVector, DeltaScalar}
import breeze.linalg.{DenseMatrix, DenseVector}

import scalagrad.spire.auto.dual.DualIsNumeric.given
import spire.math.Numeric
import spire.algebra.Trig
import spire.implicits.*
import scalagrad.showcase.deeplearning.MNISTDataSet.MNISTEntry

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


    val batchSize = 32
    val nFeatures = MNISTDataSet.nFeatures
    val nHiddenUnits = 36
    val nOutputUnits = MNISTDataSet.nLabels

    def normalize(pixel: Double) = pixel / 255

    def preprocess(data: Iterator[MNISTEntry]) = 
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

    val (xsTrain, ysTrain) = preprocess(MNISTDataSet.loadTrain)
    val (xsTest, ysTest) = preprocess(MNISTDataSet.loadTest)

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
        val exps = x.map(x => t.exp(x))
        val sumExps = exps.sum
        exps / sumExps

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
        val h = xs * firstWs + firstW0.T
        val hh = h.map(relu)
        val o = hh * lastWs + lastW0.T
        ops match 
            case oops: BreezeVectorAlgebraForDualDeltaDouble.type => 
                val fops = BreezeVectorAlgebraForDualNumberDouble
                oops.rowWiseOpsMForward(o.asInstanceOf[BreezeVectorAlgebraForDualDeltaDouble.Matrix], row => 
                    fops.transposeColumVector(stableSoftmax(fops)(fops.transposeRowVector(row)))
                ).asInstanceOf[ops.Matrix]
            // case oops: BreezeVectorAlgebraForDualDeltaDouble.type => 
            //     val stableSoftmaxV = stableSoftmax(BreezeVectorAlgebraForDouble)
            //     oops.rowWiseOpsMManual(
            //         o.asInstanceOf[BreezeVectorAlgebraForDualDeltaDouble.Matrix], 
            //         row => stableSoftmaxV(row.t).t,
            //         row => dStableSoftmax(row.t).t,
            //     ).asInstanceOf[ops.Matrix]
            case _ => o.mapRows(row => stableSoftmax(ops)(row.T).T)
        // TODO because of rowAt we lose a lot performance here...
        // val rows =
        //     for (i <- 0 until o.rows) yield stableSoftmax(ops)(o.rowAt(i).T).T
        // ops.stackRowsSeq(rows)

    def crossEntropy(ops: LinearAlgebraOps)(ys: ops.Matrix, ysHat: ops.Matrix)(using n: Numeric[ops.Scalar], t: Trig[ops.Scalar]): ops.Scalar =
        def clip(x: ops.Scalar): ops.Scalar =
            val epsilon: Double = 1e-07
            val minS = ops.liftToScalar(epsilon)
            val maxS = ops.liftToScalar(1.0 - epsilon)
            if n.lt(x, minS) then minS
            else if n.gt(x, maxS) then maxS
            else x
        ysHat.map(clip _)
        val logYsHat = ysHat.map(clip _).map(t.log)
        val logYsHatYs = logYsHat *:* ys
        -(logYsHatYs.sum / logYsHat.rows)

    val breezeOps = BreezeVectorAlgebraForDouble
    def miniBatchGradientDescent
    (data: LazyList[(breezeOps.Matrix, breezeOps.Matrix)])
    (
        firstW0: breezeOps.ColumnVector,
        firstWs: breezeOps.Matrix,
        lastW0: breezeOps.ColumnVector,
        lastWs: breezeOps.Matrix,
        alpha: breezeOps.Scalar, 
        n: Int
    )(
        createDLoss: (breezeOps.Matrix, breezeOps.Matrix) => (breezeOps.ColumnVector, breezeOps.Matrix, breezeOps.ColumnVector, breezeOps.Matrix) => (breezeOps.ColumnVector, breezeOps.Matrix, breezeOps.ColumnVector, breezeOps.Matrix)
    )(using Numeric[breezeOps.Scalar], Trig[breezeOps.Scalar]): (breezeOps.ColumnVector, breezeOps.Matrix, breezeOps.ColumnVector, breezeOps.Matrix) =
        if n == 0 then (firstW0, firstWs, lastW0, lastWs)
        else
            if (n % 100 == 0) println("Running iteration " + n)
            val (xsBatch, ysBatch) = data.head
            val dLoss = createDLoss(xsBatch, ysBatch)
            val (dFirstW0, dFirstWs, dLastW0, dLastWs) = dLoss(firstW0, firstWs, lastW0, lastWs)
            
            // val ysHatBatch = makePredictions(xsBatch)(firstW0, firstWs, lastW0, lastWs)
            // println("batch loss: " + crossEntropy(breezeOps)(ysBatch, ysHatBatch))
            // println("accuracy: " + accuracy(ysHatBatch, ysBatch) * 100)

            import breezeOps.*
            miniBatchGradientDescent(data.tail)(
                firstW0 - alpha * dFirstW0,
                firstWs - alpha * dFirstWs,
                lastW0 - alpha * dLastW0,
                lastWs - alpha * dLastWs, 
                alpha,
                n - 1
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

    import breeze.linalg.{Vector => BreezeVector, *}

    def oneHotVector(i: Int, length: Int = nOutputUnits): DenseVector[Double] =
        val res = DenseVector.zeros[Double](length)
        res(i) = 1.0
        res
        
    def lossF(ops: LinearAlgebraOps)(xs: ops.Matrix, ys: ops.Matrix)(
        firstW0: ops.ColumnVector,
        firstWs: ops.Matrix,
        lastW0: ops.ColumnVector,
        lastWs: ops.Matrix,
    )(using Numeric[ops.Scalar], Trig[ops.Scalar]): ops.Scalar =
        val ysHat = neuralNetwork(ops)(xs, firstW0, firstWs, lastW0, lastWs)
        crossEntropy(ops)(ys, ysHat)

    def accuracy(yHatProp: DenseMatrix[Double], yM: DenseMatrix[Double]): Double =
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

    val rand = scala.util.Random(42)
    
    val initFirstW0 = DenseVector.fill(nHiddenUnits)(rand.nextDouble() - 0.5)
    val initFirstWs = DenseMatrix.fill(nFeatures, nHiddenUnits)(rand.nextDouble() - 0.5)
    val initLastW0 = DenseVector.fill(nOutputUnits)(rand.nextDouble() - 0.5)
    val initLastWs = DenseMatrix.fill(nHiddenUnits, nOutputUnits)(rand.nextDouble() - 0.5)
    
    val xsM = xsTrain.head
    val ysM = ysTrain.head
    
    time {
        val epochs = 10
        val iters = epochs * MNISTDataSet.trainSize / batchSize
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
        
    }
    /*
    time {
        val iters = 1
        println(f"Start forward-mode on single batch with ${iters} iterations")
        val gradientDescentF = gradientDescent(BreezeVectorAlgebraForDouble)(
            initFirstW0, initFirstWs, initLastW0, initLastWs, 0.01, iters // 100_000
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
    }*/
    time {
        val iters = 1000
        println(f"Start reverse-mode on single batch with ${iters} iterations")
        val gradientDescentF = gradientDescent(BreezeVectorAlgebraForDouble)(
            initFirstW0, initFirstWs, initLastW0, initLastWs, 0.01, iters
        )
        
        import DeriverBreezeReversePlan.given
        val dLoss = ScalaGrad.derive(lossF(BreezeVectorAlgebraForDualDeltaDouble)(
            DualDeltaMatrix(xsM, DeltaMonad.zeroM),
            DualDeltaMatrix(ysM, DeltaMonad.zeroM)
        ))
        val (firstW0, firstWs, lastW0, lastWs) = gradientDescentF(dLoss)

        val ysHatM = makePredictions(xsM)(firstW0, firstWs, lastW0, lastWs)

        println("train acc: " + accuracy(ysHatM, ysM) * 100)
        println(f"${crossEntropy(BreezeVectorAlgebraForDouble)(ysM, ysHatM)}  -- with learned weights (${iters} iterations)")
    }
    time {
        def cycle[T](seq: Seq[T]): LazyList[T] = {
            def inner(s: Seq[T]): LazyList[T] = s match {
                case head +: tail => head #:: inner(tail)
                case _            => inner(seq)
            }
            inner(seq)
        }

        val epochs = 1
        val iters = epochs * MNISTDataSet.trainSize / batchSize
        println(f"Start reverse-mode on train batches with ${iters} iterations")
        val miniBatchGradientDescentF = miniBatchGradientDescent
            (cycle(xsTrain.zip(ysTrain)))
            (initFirstW0, initFirstWs, initLastW0, initLastWs, 0.01, iters)

        import DeriverBreezeReversePlan.given
        def createDLoss(xs: DenseMatrix[Double], ys: DenseMatrix[Double]) =
            ScalaGrad.derive(lossF(BreezeVectorAlgebraForDualDeltaDouble)(
                DualDeltaMatrix(xs, DeltaMonad.zeroM),
                DualDeltaMatrix(ys, DeltaMonad.zeroM)
            ))
        val (firstW0, firstWs, lastW0, lastWs) = miniBatchGradientDescentF(createDLoss)

        val ysHatTest = xsTest.map(xs => makePredictions(xs)(firstW0, firstWs, lastW0, lastWs))

        println("Evaluate on test set...")
        val accuracyTestBatch = ysHatTest.zip(ysTest).map((ysHat, ys) => accuracy(ysHat, ys)).toList
        val accuracyTest = accuracyTestBatch.sum / accuracyTestBatch.length
        val lossTestBatch = ysHatTest.zip(ysTest).map((ysHat, ys) => crossEntropy(BreezeVectorAlgebraForDouble)(ys, ysHat)).toList
        val lossTest = lossTestBatch.sum / lossTestBatch.length
        println("test loss: " + lossTest)
        println("test acc: " + accuracyTest * 100)
    }
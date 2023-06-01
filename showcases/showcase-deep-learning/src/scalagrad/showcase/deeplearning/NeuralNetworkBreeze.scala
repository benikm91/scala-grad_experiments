package scalagrad.showcase.deeplearning

import scala.io.Source
import scalagrad.showcase.deeplearning.Util.*
import breeze.linalg.{Vector => _, *}
import breeze.numerics._

@main def neuralNetworkBreeze() = 

    val fishs = FishDataSet.load

    val xs = {
        fishs.map(fish => Vector(fish.length1, fish.length2, fish.length3, fish.height, fish.width)).toVector
    }
    val ys = fishs.map(_.weight).toVector

    var (xs_ss, _, _) = StandardScaler.scaleMatrix(xs)
    val (ys_ss, ys_mean, ys_std) = StandardScaler.scaleColumn(ys)

    def linearModel(x: DenseVector[Double], w0: Double, ws: DenseVector[Double]): Double = 
        x.dot(ws) + w0

    def relu(x: Double): Double = 
        if x < 0 then 0 else x

    def neuralNetwork(
        x: DenseVector[Double],
        firstW0: DenseVector[Double], 
        firstWs: DenseMatrix[Double],
        lastW0: Double,
        lastWs: DenseVector[Double],
    ): Double = 
        val h = (firstWs * x + firstW0).map(relu)
        linearModel(h, lastW0, lastWs)

    def loss(ys: DenseVector[Double], ysHat: DenseVector[Double]): Double =
        sum((ys - ysHat).map(x => x * x)) / ys.size / 2

    def backprop(
        xs: DenseMatrix[Double], 
        ys: DenseVector[Double], 
        firstW0: DenseVector[Double],
        firstWs: DenseMatrix[Double],
        lastW0: Double,
        lastWs: DenseVector[Double],
    ): (DenseVector[Double], DenseMatrix[Double], Double, DenseVector[Double]) =
        def dRelu(z: Double): Double = if z <= 0 then 0 else 1
        val n = xs.rows.toDouble
        val h1 = firstWs * xs.t
        val h2 = h1(::, *) + firstW0
        val hs = h2.map(relu)
        val ysHat: DenseVector[Double] = (lastWs.t * hs).t + lastW0
        val dLastW0: Double = sum(ysHat - ys) / n
        val dLastWs: DenseVector[Double] = hs * (ysHat - ys) / n
        val dH_dLoss: DenseMatrix[Double] = lastWs * (ysHat - ys).t
        val dH2_dH = h2.map(x => dRelu(x))
        val dH2_dLoss: DenseMatrix[Double] = dH_dLoss *:* dH2_dH
        // (yHat - y) * lastWs(i) * dRelu(hInput(i))
        val dFirstW0: DenseVector[Double] = sum(dH2_dLoss, Axis._1) / n
        val dFirstWs: DenseMatrix[Double] = (dH2_dLoss * xs) / n
        (dFirstW0, dFirstWs, dLastW0, dLastWs)

    def gradientDescent(
        xs: DenseMatrix[Double], 
        ys: DenseVector[Double], 
        firstW0: DenseVector[Double],
        firstWs: DenseMatrix[Double],
        lastW0: Double,
        lastWs: DenseVector[Double],
        alpha: Double, 
        n: Int
    ): (DenseVector[Double], DenseMatrix[Double], Double, DenseVector[Double]) =
        if n == 0 then (firstW0, firstWs, lastW0, lastWs)
        else
            val (dFirstW0, dFirstWs, dLastW0, dLastWs) = backprop(xs, ys, firstW0, firstWs, lastW0, lastWs)
            gradientDescent(
                xs, 
                ys, 
                firstW0 - alpha * dFirstW0,
                firstWs - alpha * dFirstWs,
                lastW0 - alpha * dLastW0,
                lastWs - alpha * dLastWs, 
                alpha,
                n - 1
            )

    val rand = scala.util.Random(42)
    
    val nFeatures = xs_ss(0).size
    val nHiddenUnits = 6
    val initFirstW0 = DenseVector.fill(nHiddenUnits)(rand.nextDouble())
    val initFirstWs = DenseMatrix.fill(nFeatures, nHiddenUnits)(rand.nextDouble()).t
    val initLastW0 = rand.nextDouble()
    val initLastWs = DenseVector.fill(nHiddenUnits)(rand.nextDouble())

    val xsSS = new DenseMatrix(xs_ss.head.length, xs_ss.length, xs_ss.flatten.toArray).t
    val ysSS = DenseVector(ys_ss.toArray)

    val initYsHat = StandardScaler.inverseScaleColumn( 
        (0 until xsSS.rows).map { i =>
            val row = xsSS(i, ::).t
            neuralNetwork(row, initFirstW0, initFirstWs, initLastW0, initLastWs)
        }.toVector, 
        ys_mean, ys_std
    )
    
    println(f"${Math.sqrt(loss(DenseVector(ys.toArray), DenseVector(initYsHat.toArray)))}g  -- RMSE with initial weights")
    
    time {
        (1 to 1 by 1).foreach(_ => 
            time {
                val (firstW0, firstWs, lastW0, lastWs) = gradientDescent(
                    xsSS, ysSS,
                    initFirstW0, 
                    initFirstWs,
                    initLastW0,
                    initLastWs, 
                    0.01, 
                    100_000
                )

                val ysHat = StandardScaler.inverseScaleColumn(
                    (0 until xsSS.rows).map { i =>
                        val row = xsSS(i, ::).t
                        neuralNetwork(row, firstW0, firstWs, lastW0, lastWs)
                    }.toVector, 
                    ys_mean, ys_std
                )
                
                println(f"${Math.sqrt(loss(DenseVector(ys.toArray), DenseVector(ysHat.toArray)))}g  -- RMSE with learned weights")
            }
        )
    }
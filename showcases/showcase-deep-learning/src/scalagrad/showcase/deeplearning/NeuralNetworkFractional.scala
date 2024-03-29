package scalagrad.showcase.deeplearning

import scala.io.Source
import scalagrad.api.ScalaGrad

import scalagrad.fractional.auto.dual.DualIsFractional.given

import scalagrad.showcase.deeplearning.Util.*

import scalagrad.fractional.auto.dual.DualIsFractional

import scalagrad.auto.forward.DeriverForwardPlan

@main def neuralNetworkFractional() = 

    val nHiddenUnits = 6

    val fishs = FishDataSet.load

    val xs = fishs.map(fish => Vector(fish.length1, fish.length2, fish.length3, fish.height, fish.width)).toVector
    val ys = fishs.map(_.weight).toVector

    val nFeatures = xs(0).size

    val (xs_ss, _, _) = StandardScaler.scaleMatrix(xs)
    val (ys_ss, ys_mean, ys_std) = StandardScaler.scaleColumn(ys)

    def linearModel[T](x: Vector[T], w0: T, ws: Vector[T])(using f: Fractional[T]): T =
        import f.* 
        w0 + x.zip(ws).map(_ * _).sum

    def relu[T](x: T)(using f: Fractional[T]): T =
        import f.* 
        if f.toDouble(x) < 0 then f.zero else x

    def neuralNetwork[T: Fractional](
        x: Vector[T], 
        firstW0: Vector[T],
        firstWs: Vector[Vector[T]],
        lastW0: T,
        lastWs: Vector[T],
    ): T = 
        var a = x
        a = firstW0.zip(firstWs).map((w0, ws) => linearModel(a, w0, ws)).map(relu)
        linearModel(a, lastW0, lastWs)

    def checkConsistency[T](firstW0: Vector[T], firstWs: Vector[Vector[T]], lastW0: T, lastWs: Vector[T]) =
        require(firstWs.forall(_.size == nFeatures), s"first layer must be consistent with nFeatures ${firstWs.map(_.size).mkString(",")} =each= ${nFeatures}")
        require(lastWs.size == firstW0.size, s"second layer must be consistent with first layer ${lastWs.size} == ${firstW0.size}")
        require(firstW0.size == nHiddenUnits, "first layer (bias) must be consistent with nHiddenUnits")
        require(firstWs.size == nHiddenUnits, "first layer must be consistent with nHiddenUnits")

    def unstackWeights[T](ws: Vector[T]) = 
        val firstW0 = ws.take(nHiddenUnits)
        val firstWs = ws.drop(nHiddenUnits).take(nFeatures * nHiddenUnits).grouped(nFeatures).toVector
        val lastW0 = ws.drop(nHiddenUnits + nFeatures * nHiddenUnits).head
        val lastWs = ws.drop(nHiddenUnits + nFeatures * nHiddenUnits).tail
        checkConsistency(firstW0, firstWs, lastW0, lastWs)
        (firstW0, firstWs, lastW0, lastWs)

    def stackWeights[T](firstW0: Vector[T], firstWs: Vector[Vector[T]], lastW0: T, lastWs: Vector[T]) =
        checkConsistency(firstW0, firstWs, lastW0, lastWs)
        firstW0 ++ firstWs.flatten ++ Vector(lastW0) ++ lastWs

    def loss[T](ys: Vector[T], ysHat: Vector[T])(using f: Fractional[T]): T = 
        import f.*
        ys.zip(ysHat).map { case (y, yHat) => 
            (y - yHat) * (y - yHat)
        }.sum / fromInt(ys.size * 2)

    def lossF[T: Fractional](xs: Vector[Vector[T]], ys: Vector[T])(
        ws: Vector[T]
    ): T =
        val (firstW0, firstWs, lastW0, lastWs) = unstackWeights(ws)
        val ysHat = xs.map(x => neuralNetwork(x, firstW0, firstWs, lastW0, lastWs))
        loss(ys, ysHat)

    def gradientDescentF(
        xs: Vector[Vector[Double]], 
        ys: Vector[Double], 
        firstW0: Vector[Double],
        firstWs: Vector[Vector[Double]],
        lastW0: Double,
        lastWs: Vector[Double],
        alpha: Double, 
        n: Int
    )(
        dLoss: Vector[Double] => Vector[Double]
    ): (Vector[Double], Vector[Vector[Double]], Double, Vector[Double]) =
        if n == 0 then (firstW0, firstWs, lastW0, lastWs)
        else
            val (dFirstW0, dFirstWs, dLastW0, dLastWs) = unstackWeights(
                dLoss(stackWeights(firstW0, firstWs, lastW0, lastWs))
            )
            gradientDescentF(
                xs, 
                ys, 
                firstW0.zip(dFirstW0).map { case (w0, dw0) => w0 - alpha * dw0 }, 
                firstWs.zip(dFirstWs).map { case (ws, dws) => ws.zip(dws).map { case (w, dw) => w - alpha * dw } }, 
                lastW0 - alpha * dLastW0, 
                lastWs.zip(dLastWs).map { case (w, dw) => w - alpha * dw }, 
                alpha, 
                n - 1
            )(dLoss)

    val rand = scala.util.Random(42)
    
    val initFirstW0 = Vector.fill(nHiddenUnits)(rand.nextDouble())
    val initFirstWs = Vector.fill(nHiddenUnits)(Vector.fill(nFeatures)(rand.nextDouble()))
    val initLastW0 = rand.nextDouble()
    val initLastWs = Vector.fill(nHiddenUnits)(rand.nextDouble())

    val initYHat = xs.map(x => neuralNetwork(x, initFirstW0, initFirstWs, initLastW0, initLastWs))
    println(f"${Math.sqrt(loss(ys, initYHat))}g  -- RMSE with initial weights")
    
    val gradientDescent = gradientDescentF(xs_ss, ys_ss, initFirstW0, initFirstWs, initLastW0, initLastWs, 0.01, 100) _

    time {
        import scalagrad.auto.forward.dual.DualNumber
        import scalagrad.auto.forward.dual.DualNumber.given
        import scalagrad.auto.forward.DeriverForwardPlan.given

        println("Forward mode")
        val dLoss = ScalaGrad.derive(lossF[DualNumber[Double]](
            xs_ss.map(_.map(DualNumber(_, 0.0))), 
            ys_ss.map(DualNumber(_, 0.0)
        )))
        val (initFirstW0, initFirstWs, lastW0, lastWs) = gradientDescent(dLoss)
        val ysHat = StandardScaler.inverseScaleColumn(xs_ss.map(neuralNetwork(_, initFirstW0, initFirstWs, lastW0, lastWs)), ys_mean, ys_std)
        println(f"${Math.sqrt(loss(ys, ysHat))}g  -- RMSE with learned weights")
    }

    time {
        import scalagrad.auto.reverse.dual.DualDelta
        import scalagrad.auto.reverse.dual.DualDelta.given
        import scalagrad.auto.reverse.DeriverReversePlan.given

        println("Reverse mode")
        val dLoss = ScalaGrad.derive(lossF[DualDelta[Double]](
            xs_ss.map(_.map(DualDelta(_, DualDelta.ZeroM[Double]))), 
            ys_ss.map(DualDelta(_, DualDelta.ZeroM[Double])
        )))
        val (initFirstW0, initFirstWs, lastW0, lastWs) = gradientDescent(dLoss)
        val ysHat = StandardScaler.inverseScaleColumn(xs_ss.map(neuralNetwork(_, initFirstW0, initFirstWs, lastW0, lastWs)), ys_mean, ys_std)
        println(f"${Math.sqrt(loss(ys, ysHat))}g  -- RMSE with learned weights")
    }
package scalagrad.showcase.deeplearning

import scalagrad.api.ScalaGrad

import scalagrad.fractional.auto.dual.DualIsFractional.given


import scalagrad.showcase.deeplearning.Util.*

import scalagrad.fractional.auto.dual.DualIsFractional

import scalagrad.auto.forward.DeriverForwardPlan


@main def linearRegressionAutoDiff() = 

    val fishs = FishDataSet.load

    val xsRaw = fishs.map(fish => Vector(fish.length1, fish.length2, fish.length3, fish.height, fish.width)).toVector
    def generatePolynomialFeatures(x: Vector[Double], degree: Int): Vector[Double] =
        if degree == 1 then x
        else
            generatePolynomialFeatures(x ++ x.zip(x).map { case (x1, x2) => x1 * x2 }, degree = degree - 1)
    val xsPolynom = xsRaw.map(generatePolynomialFeatures(_, 3))
    val xs = xsRaw
    // val xs = xsPolynom
    val ys = fishs.map(_.weight).toVector
    println(f"Samples: ${xs.size}, Features ${xs(0).size}")

    val (xs_ss, _, _) = StandardScaler.scaleMatrix(xs)
    val (ys_ss, ys_mean, ys_std) = StandardScaler.scaleColumn(ys)

    def predict[T](x: Vector[T], w0: T, ws: Vector[T])(using f: Fractional[T]): T = 
        import f.*
        w0 + x.zip(ws).map(_ * _).sum

    def loss[T](ys: Vector[T], ysHat: Vector[T])(using f: Fractional[T]): T =
        import f.*
        ys.zip(ysHat).map { case (y, yHat) => 
            (y - yHat) * (y - yHat)
        }.sum / fromInt(ys.size * 2)

    def lossF[T: Fractional](xs: Vector[Vector[T]], ys: Vector[T])(w0Andws: Vector[T]): T =
        val w0 = w0Andws.head
        val ws = w0Andws.tail
        loss(ys, xs.map(predict(_, w0, ws)))

    def gradientDescentF(w0: Double, ws: Vector[Double], alpha: Double, n: Int)(dLoss: Vector[Double] => Vector[Double]): (Double, Vector[Double]) =
        if n == 0 then (w0, ws)
        else
            val dW0AndW1 = dLoss(w0 +: ws)
            val (dw0, dws) = (dW0AndW1.head, dW0AndW1.tail)
            gradientDescentF(w0 - alpha * dw0, ws.zip(dws).map { case (w, dw) => w - alpha * dw }, alpha, n - 1)(dLoss)

    val (initW0, initWs) = (0.0, Vector.fill(xs(0).size)(0.0))
    
    val initYsHat = StandardScaler.inverseScaleColumn(xs_ss.map(predict(_, initW0, initWs)), ys_mean, ys_std)
    println(f"${Math.sqrt(loss(ys, initYsHat))}g  -- RMSE with initial weights")
    val gradientDescent = gradientDescentF(initW0, initWs, 0.01, 1000)
    time {
        import scalagrad.auto.forward.dual.DualNumber
        import scalagrad.auto.forward.dual.DualNumber.given
        import scalagrad.auto.forward.DeriverForwardPlan.given

        println("Forward mode")
        val dLoss = ScalaGrad.derive(lossF[DualNumber[Double]](
            xs_ss.map(_.map(DualNumber(_, 0.0))), 
            ys_ss.map(DualNumber(_, 0.0)
        )))
        val (w0, ws) = gradientDescent(dLoss)
        val ysHat = StandardScaler.inverseScaleColumn(xs_ss.map(predict(_, w0, ws)), ys_mean, ys_std)
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
        val (w0, ws) = gradientDescent(dLoss)
        val ysHat = StandardScaler.inverseScaleColumn(xs_ss.map(predict(_, w0, ws)), ys_mean, ys_std)
        println(f"${Math.sqrt(loss(ys, ysHat))}g  -- RMSE with learned weights")
    }

package scalagrad.showcase.deeplearning

import scalagrad.showcase.deeplearning.Util.*

@main def newtonMethod() = 

    val fishs = FishDataSet.load

    val xs = fishs.map(fish => Vector(fish.length1, fish.length2, fish.length3, fish.height, fish.width)).toVector
    val ys = fishs.map(_.weight).toVector

    val (xs_ss, _, _) = StandardScaler.scaleMatrix(xs)
    val (ys_ss, ys_mean, ys_std) = StandardScaler.scaleColumn(ys)

    def predict(x: Vector[Double], w0: Double, ws: Vector[Double]): Double = 
        w0 + x.zip(ws).map(_ * _).sum

    def loss(ys: Vector[Double], ysHat: Vector[Double]): Double =
        ys.zip(ysHat).map { case (y, yHat) => 
            Math.pow(y - yHat, 2)
        }.sum / ys.size / 2

    // first order derivative
    def dLoss(xs: Vector[Vector[Double]], ys: Vector[Double], w0: Double, ws: Vector[Double]): (Double, Vector[Double]) =
        val dw0 = xs.zip(ys).map { case (x, y) => y - predict(x, w0, ws) }.sum / xs.size
        val dws = ws.zipWithIndex.map { case (w, i) => w - xs.zip(ys).map { case (x, y) => (y - predict(x, w0, ws)) * x(i) }.sum / xs.size }
        (dw0, dws)
    
    // second order derivative
    def d2Loss(xs: Vector[Vector[Double]], ys: Vector[Double], w0: Double, ws: Vector[Double]): (Vector[Double], Vector[Vector[Double]]) =
        val dLossW0 = xs.zip(ys).map { case (x, y) => y - predict(x, w0, ws) }.sum / xs.size
        val d2LossW0 = ???
        val dLossWs = ws.zipWithIndex.map { case (w, i) => w - xs.zip(ys).map { case (x, y) => (y - predict(x, w0, ws)) * x(i) }.sum / xs.size }
        val d2LossWs = ???
        (d2LossW0, d2LossWs)


    def newtonMethod(xs: Vector[Vector[Double]], ys: Vector[Double], w0: Double, ws: Vector[Double], alpha: Double, n: Int): (Double, Vector[Double]) =
        if n == 0 then (w0, ws)
        else
            val (dw0, dws) = dLoss(xs, ys, w0, ws)
            newtonMethod(xs, ys, w0 - alpha * dw0, ws.zip(dws).map { case (w, dw) => w - alpha * dw }, alpha, n - 1)

    val (initW0, initWs) = (0.0, Vector.fill(xs(0).size)(0.0))
    
    val initYsHat = StandardScaler.inverseScaleColumn(xs_ss.map(predict(_, initW0, initWs)), ys_mean, ys_std)
    println(f"${Math.sqrt(loss(ys, initYsHat))}g  -- RMSE with initial weights")
    time {
        val (w0, ws) = newtonMethod(xs_ss, ys_ss, initW0, initWs, 0.01, 100)
        val ysHat = StandardScaler.inverseScaleColumn(xs_ss.map(predict(_, w0, ws)), ys_mean, ys_std)
        println(f"${Math.sqrt(loss(ys, ysHat))}g  -- RMSE with learned weights")
    }

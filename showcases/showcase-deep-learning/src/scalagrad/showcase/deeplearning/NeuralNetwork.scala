package scalagrad.showcase.deeplearning

import scala.io.Source

@main def neuralNetwork() = 

    val fishs = FishDataSet.load

    val xs = fishs.map(fish => Vector(fish.length1, fish.length2, fish.length3, fish.height, fish.width)).toVector
    val ys = fishs.map(_.weight).toVector

    val (xs_ss, _, _) = StandardScaler.scaleMatrix(xs)
    val (ys_ss, ys_mean, ys_std) = StandardScaler.scaleColumn(ys)

    def linearModel(x: Vector[Double], w0: Double, ws: Vector[Double]): Double = 
        w0 + x.zip(ws).map(_ * _).sum

    def sigmoid(x: Double): Double = 
        1 / (1 + Math.exp(-x))

    def relu(x: Double): Double = 
        if x < 0 then 0 else x

    def neuralNetwork(
        x: Vector[Double], 
        firstW0: Vector[Double],
        firstWs: Vector[Vector[Double]],
        lastW0: Double,
        lastWs: Vector[Double],
    ): Double = 
        var a = x
        a = firstW0.zip(firstWs).map((w0, ws) => linearModel(a, w0, ws)).map(relu)
        linearModel(a, lastW0, lastWs)

    def loss(ys: Vector[Double], ysHat: Vector[Double]): Double =
        ys.zip(ysHat).map { case (y, yHat) => 
            Math.pow(y - yHat, 2)
        }.sum / ys.size / 2

    def backprop(
        xs: Vector[Vector[Double]], 
        ys: Vector[Double], 
        firstW0: Vector[Double],
        firstWs: Vector[Vector[Double]],
        lastW0: Double,
        lastWs: Vector[Double],
    ): (Vector[Double], Vector[Vector[Double]], Double, Vector[Double]) =
        var dFirstW0 = Array.fill(firstW0.size)(0.0)
        var dFirstWs = Array.fill(firstWs.size)(Vector.fill(firstWs(0).size)(0.0))
        var dLastW0 = 0.0
        var dLastWs = Array.fill(lastWs.size)(0.0)
        for (x, y) <- xs.zip(ys) do
            var a = x
            val aList = a :: Nil
            val zList = Nil
            a = firstW0.zip(firstWs).map((w0, ws) => linearModel(a, w0, ws)).map(relu)
            val z = linearModel(a, lastW0, lastWs)
            val yHat = z
            val dLastZ = yHat - y
            dLastW0 += dLastZ
            dLastWs = dLastWs.zip(a).map((dw, a) => dw + dLastZ * a)
            for (i <- firstW0.indices) do
                val dFirstZ = dLastZ * lastWs(i) * relu(a(i))
                dFirstW0(i) += dFirstZ
                dFirstWs(i) = dFirstWs(i).zip(x).map((dw, x) => dw + dFirstZ * x)
        (dFirstW0.map(_ / xs.size).toVector, dFirstWs.map(_.map(_ / xs.size)).toVector, dLastW0 / xs.size, dLastWs.map(_ / xs.size).toVector)

    def gradientDescent(
        xs: Vector[Vector[Double]], 
        ys: Vector[Double], 
        firstW0: Vector[Double],
        firstWs: Vector[Vector[Double]],
        lastW0: Double,
        lastWs: Vector[Double],
        alpha: Double, 
        n: Int
    ): (Vector[Double], Vector[Vector[Double]], Double, Vector[Double]) =
        if n == 0 then (firstW0, firstWs, lastW0, lastWs)
        else
            val (dFirstW0, dFirstWs, dLastW0, dLastWs) = backprop(xs, ys, firstW0, firstWs, lastW0, lastWs)
            gradientDescent(
                xs, 
                ys, 
                firstW0.zip(dFirstW0).map { case (w0, dw0) => w0 - alpha * dw0 }, 
                firstWs.zip(dFirstWs).map { case (ws, dws) => ws.zip(dws).map { case (w, dw) => w - alpha * dw } }, 
                lastW0 - alpha * dLastW0, 
                lastWs.zip(dLastWs).map { case (w, dw) => w - alpha * dw }, 
                alpha, 
                n - 1
            )

    val rand = scala.util.Random
    
    val nHiddenUnits = 5
    val initFirstW0 = Vector.fill(nHiddenUnits)(rand.nextDouble())
    val initFirstWs = Vector.fill(nHiddenUnits)(Vector.fill(xs(0).size)(rand.nextDouble()))
    val initLastW0 = rand.nextDouble()
    val initLastWs = Vector.fill(nHiddenUnits)(rand.nextDouble())

    val (firstW0, firstWs, lastW0, lastWs) = gradientDescent(
        xs_ss, ys_ss, 
        initFirstW0, initFirstWs, 
        initLastW0, initLastWs, 
        0.01, 
        10000
    )

    println((initFirstW0, firstW0))

    val initYsHat = StandardScaler.inverseScaleColumn(xs_ss.map(x => neuralNetwork(x, initFirstW0, initFirstWs, initLastW0, initLastWs)), ys_mean, ys_std)
    println(f"${Math.sqrt(loss(ys, initYsHat))}g  -- RMSE with initial weights")
    val ysHat = StandardScaler.inverseScaleColumn(xs_ss.map(x => neuralNetwork(x, firstW0, firstWs, lastW0, lastWs)), ys_mean, ys_std)
    println(f"${Math.sqrt(loss(ys, ysHat))}g  -- RMSE with learned weights")
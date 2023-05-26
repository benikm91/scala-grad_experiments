package scalagrad.showcase.deeplearning

import scala.io.Source
import scalagrad.showcase.deeplearning.Util.*
import breeze.linalg.{Vector => _, *}
import breeze.numerics._

@main def linearRegressionBreeze() = 

    val fishs = FishDataSet.load

    val xs = {
        fishs.map(fish => Vector(fish.length1, fish.length2, fish.length3, fish.height, fish.width)).toVector
    }
    val ys = fishs.map(_.weight).toVector

    var (xs_ss, _, _) = StandardScaler.scaleMatrix(xs)
    val (ys_ss, ys_mean, ys_std) = StandardScaler.scaleColumn(ys)

    def predict(x: DenseMatrix[Double], w0: Double, ws: DenseVector[Double]): DenseVector[Double] = 
        (x * ws) + w0

    def loss(ys: DenseVector[Double], ysHat: DenseVector[Double]): Double =
        sum((ys - ysHat).map(x => x * x)) / ys.size / 2

    def dLoss(
        xs: DenseMatrix[Double], 
        ys: DenseVector[Double], 
        w0: Double,
        ws: DenseVector[Double],
    ): (Double, DenseVector[Double]) =
        val yHat = predict(xs, w0, ws)
        val dW0 = sum(yHat - ys) / ys.size.toDouble
        val dWs = ((yHat - ys).t * xs / ys.size.toDouble).t
        (dW0, dWs)

    def gradientDescent(
        xs: DenseMatrix[Double], 
        ys: DenseVector[Double], 
        w0: Double,
        ws: DenseVector[Double],
        alpha: Double, 
        n: Int
    ): (Double, DenseVector[Double]) =
        if n == 0 then (w0, ws)
        else
            val (dW0, dWs) = dLoss(xs, ys, w0, ws)
            gradientDescent(
                xs, 
                ys, 
                w0 - alpha * dW0,
                ws - alpha * dWs,
                alpha,
                n - 1
            )

    val (initW0, initWs) = (0.0, DenseVector.fill(xs(0).size)(0.0))

    val xsSS = new DenseMatrix(xs_ss.head.length, xs_ss.length, xs_ss.flatten.toArray).t
    val ysSS = DenseVector(ys_ss.toArray)
    
    val initYsHat = StandardScaler.inverseScaleColumn(predict(xsSS, initW0, initWs).toScalaVector, ys_mean, ys_std)
    println(f"${Math.sqrt(loss(DenseVector(ys.toArray), DenseVector(initYsHat.toArray)))}g  -- RMSE with initial weights")

    time {
        val (w0, ws) = gradientDescent(xsSS, ysSS, initW0, initWs, 0.01, 100)
        val ysHat = StandardScaler.inverseScaleColumn(predict(xsSS, w0, ws).toScalaVector, ys_mean, ys_std)
        println(f"${Math.sqrt(loss(DenseVector(ys.toArray), DenseVector(ysHat.toArray)))}g  -- RMSE with learned weights")
    }



    


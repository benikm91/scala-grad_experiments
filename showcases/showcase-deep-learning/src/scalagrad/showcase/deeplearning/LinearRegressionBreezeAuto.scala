package scalagrad.showcase.deeplearning

import scala.io.Source
import scalagrad.showcase.deeplearning.Util.*
import breeze.linalg.{Vector => _, *}
import breeze.numerics._

import scalagrad.tensor.breeze.auto.DualNumberDenseVector
import scalagrad.tensor.breeze.auto.DualNumberDenseMatrix
import scalagrad.auto.forward.dual.DualNumber

import scalagrad.auto.forward.DeriverForwardPlan.given
import scalagrad.fractional.auto.dual.DualIsFractional.given
import scalagrad.api.ScalaGrad

import scalagrad.tensor.breeze.auto.DualNumberDenseVectorCapabilities
import scalagrad.tensor.breeze.auto.DualNumberDenseMatrixCapabilities

@main def linearRegressionBreezeAuto() = 

    type DS[T] = DualNumber[T]
    type DV[T] = DualNumberDenseVector[T]
    type DM[T] = DualNumberDenseMatrix[T]

    val fishs = FishDataSet.load

    val xs = {
        fishs.map(fish => Vector(fish.length1, fish.length2, fish.length3, fish.height, fish.width)).toVector
    }
    val ys = fishs.map(_.weight).toVector

    var (xs_ss, _, _) = StandardScaler.scaleMatrix(xs)
    val (ys_ss, ys_mean, ys_std) = StandardScaler.scaleColumn(ys)

    def predictBreeze(x: DenseMatrix[Double], w0: Double, ws: DenseVector[Double]): DenseVector[Double] = 
        (x * ws) + w0

    def predict(x: DM[Double], w0: DS[Double], ws: DV[Double]): DV[Double] = 
        (x * ws) + w0

    def square[T: Fractional](x: T): T = 
        val f = summon[Fractional[T]]
        f.times(x, x)

    def stackBias(w0: Double, ws: DenseVector[Double]): DenseVector[Double] =
        DenseVector(w0 +: ws.toArray)

    def unstackBias(w0AndWs: DV[Double]): (DS[Double], DV[Double]) = {
        val w0 = DualNumber(w0AndWs.value(0), w0AndWs.derivative(0))
        val ws = DualNumberDenseVector(w0AndWs.value.slice(1, w0AndWs.length), w0AndWs.derivative.slice(1, w0AndWs.length))
        (w0, ws)
    }

    def unstackBias2(w0AndWs: DenseVector[Double]): (Double, DenseVector[Double]) =
        (w0AndWs(0), w0AndWs(1 to -1))

    def loss(xs: DM[Double], ys: DV[Double])(w0: DS[Double], ws: DV[Double]): DS[Double] =
        val ysHat = predict(xs, w0, ws)
        val squareDouble = square[Double]
        val dSquare = ScalaGrad.derive(square[DualNumber[Double]]) // TODO move this inside "framework"
        val temp = (ys - ysHat).map(square, dSquare).sum  // TODO can we use Breeze sum directly here?
        temp * DualNumber((1.0 / ys.length.toDouble / 2), 0.0)

    def loss2(xs: DM[Double], ys: DV[Double])(w0AndWs: DV[Double]): DS[Double] =
        unstackBias(w0AndWs) match
            case (w0, ws) => 
                loss(xs, ys)(w0, ws)

    def lossBreeze(ys: DenseVector[Double], ysHat: DenseVector[Double]): Double =
        sum((ys - ysHat).map(square)) / ys.length.toDouble / 2

    def gradientDescent(
        xs: DenseMatrix[Double], 
        ys: DenseVector[Double], 
        w0: Double,
        ws: DenseVector[Double],
        alpha: Double, 
        n: Int
    )(
        dLoss: (Double, DenseVector[Double]) => (Double, DenseVector[Double])
    ): (Double, DenseVector[Double]) =
        if n == 0 then (w0, ws)
        else
            val (dW0, dWs) = dLoss(w0, ws)
            gradientDescent(
                xs, 
                ys, 
                w0 - dW0 * alpha,
                ws - dWs * alpha,
                alpha,
                n - 1
            )(dLoss)

    val (initW0, initWs) = (0.0, DenseVector.fill(xs(0).size)(0.0))

    val xsSS = new DenseMatrix(xs_ss.head.length, xs_ss.length, xs_ss.flatten.toArray).t
    val ysSS = DenseVector(ys_ss.toArray)
    
    import scalagrad.tensor.breeze.auto.DeriverTensorForwardPlan.given

    summon[scalagrad.api.Deriver[scalagrad.tensor.breeze.auto.DualNumberDenseVector[Double] => DualNumber[Double]]]

    time {
        val dLoss2: DenseVector[Double] => DenseVector[Double] = ScalaGrad.derive(loss2(
            DualNumberDenseMatrix.valueOnly(xsSS),
            DualNumberDenseVector.valueOnly(ysSS)
        ))
        val dLoss = (w0: Double, ws: DenseVector[Double]) => 
            unstackBias2(dLoss2(stackBias(w0, ws)))
        val (w0, ws) = gradientDescent(xsSS, ysSS, initW0, initWs, 0.01, 100_000)(dLoss)
        val ysHat = StandardScaler.inverseScaleColumn(predictBreeze(xsSS, w0, ws).toScalaVector, ys_mean, ys_std)
        println(f"${Math.sqrt(lossBreeze(DenseVector(ys.toArray), DenseVector(ysHat.toArray)))}g  -- RMSE with learned weights")
    }



    


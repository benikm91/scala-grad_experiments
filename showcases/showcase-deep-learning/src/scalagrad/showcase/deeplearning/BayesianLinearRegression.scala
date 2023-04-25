package scalagrad.showcase.deeplearning

import scalagrad.api.ScalaGrad

import spire.math.Numeric
import spire.algebra.Trig
import spire.implicits.*
import spire.compat.numeric

import scalagrad.auto.forward.dual.DualNumber
import scalagrad.auto.forward.dual.DualNumber.given
import scalagrad.spire.auto.dual.DualIsNumeric.given
import scalagrad.spire.auto.dual.DualIsNumeric
import scalagrad.auto.forward.DeriverForwardPlan
import scalagrad.auto.forward.DeriverForwardPlan.given

extension [T: Numeric](x1: Int)
    def *(x2: T): T = 
        summon[Numeric[T]].fromInt(x1) * x2

@main def bayesianLinearRegression() = 
    def logLikelihoodNormalUnnormalized[T: Numeric: Trig](y: Vector[T], X: Vector[Vector[T]], sigma: T)(beta: Vector[T]): T = 
        val trig = summon[Trig[T]]
        val num = summon[Numeric[T]]
        val n = y.length
        val p = beta.length
        val yHat = X.map(row => row.zip(beta).map(_ * _).sum)
        val residuals = y.zip(yHat).map(_ - _)
        val error = residuals.map(x => x * x).sum
        -(n / 2 * trig.log(2 * trig.pi * sigma * sigma)) - error * 1 / (2 * sigma * sigma)

    def logPriorLogNormal[T: Numeric: Trig](sigma: T, mu: T)(beta: Vector[T]): T = {
        val trig = summon[Trig[T]]
        val num = summon[Numeric[T]]
        val p = beta.length
        val nonConst = num.one / (2 * sigma * sigma) * beta.map(_ - mu).map(x => x * x).sum
        val const = - (p / 2) * trig.log(2 * trig.pi * sigma * sigma)
        -const - nonConst
    }

    def logPosteriorUnnormalized[T: Numeric: Trig](logLikelihoodUnnormalized: Vector[T] => T, logPriorUnnormalized: Vector[T] => T) =
        (beta: Vector[T]) => 
            logLikelihoodUnnormalized(beta) + logPriorUnnormalized(beta)

    def gradientAscentF(ws: Vector[Double], alpha: Double, n: Int)(dLoss: Vector[Double] => Vector[Double]): Vector[Double] =
        if n == 0 then ws
        else
            val dws = dLoss(ws)
            gradientAscentF(ws.zip(dws).map(_ + alpha * _), alpha, n - 1)(dLoss)

    val fishs = FishDataSet.load
    val y = fishs.map(_.weight).toVector
    val xs = fishs.map(fish => Vector(fish.length1, fish.length2, fish.length3, fish.height, fish.width)).toVector

    val (xs_ss_wb, _, _) = StandardScaler.scaleMatrix(xs)
    
    // add dimension for bias
    val xs_ss = xs_ss_wb.map(x => 1.0 +: x)

    val (ys_ss, ys_mean, ys_std) = StandardScaler.scaleColumn(y)

    val ys_ss_DN = ys_ss.map(DualNumber(_, 0.0))
    val xs_ss_DN = xs_ss.map(_.map(DualNumber(_, 0.0)))
    val posterior = logPosteriorUnnormalized[DualNumber[Double]](logLikelihoodNormalUnnormalized(ys_ss_DN, xs_ss_DN, DualNumber(1.0, 0.0)), logPriorLogNormal(DualNumber(1, 0.0), DualNumber(0.0, 0.0)))

    val initialBeta = Vector.fill(xs(0).length)(0.0)
    val gradientAscent = gradientAscentF(initialBeta, 0.0001, 10000)


    def predict[T](x: Vector[T], ws: Vector[T])(using f: Fractional[T]): T = 
        import f.*
        x.zip(ws).map(_ * _).sum

    def loss[T](ys: Vector[T], ysHat: Vector[T])(using f: Fractional[T]): T =
        import f.*
        ys.zip(ysHat).map { case (y, yHat) => 
            (y - yHat) * (y - yHat)
        }.sum / fromInt(ys.size * 2)

    {
        val yHat = StandardScaler.inverseScaleColumn(xs_ss.map(x => predict(x, initialBeta)), ys_mean, ys_std)
        val lossValue = loss(y, yHat)
        println(f"${Math.sqrt(lossValue)} -- with initial weights")
    }

    val beta = gradientAscent(ScalaGrad.derive(posterior))
    val yHat = StandardScaler.inverseScaleColumn(xs_ss.map(x => predict(x, beta)), ys_mean, ys_std)
    println(f"${Math.sqrt(loss(y, yHat))}g  -- RMSE with learned weights")

    

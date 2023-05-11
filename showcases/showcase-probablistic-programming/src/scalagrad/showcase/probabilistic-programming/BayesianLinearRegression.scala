package scalagrad.showcase.probabilisticProgramming

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
import breeze.stats.distributions.Rand.FixedSeed.randBasis
import scala.math.{sqrt, exp, min}
import breeze.linalg.{DenseVector, DenseMatrix}

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
            (logLikelihoodUnnormalized(beta) + logPriorUnnormalized(beta))

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
    val posteriorDouble = logPosteriorUnnormalized[Double](logLikelihoodNormalUnnormalized(ys_ss, xs_ss, 1.0), logPriorLogNormal(1.0, 0.0))

    def predict[T](x: Vector[T], ws: Vector[T])(using f: Fractional[T]): T = 
        import f.*
        x.zip(ws).map(_ * _).sum

    def loss[T](ys: Vector[T], ysHat: Vector[T])(using f: Fractional[T]): T =
        import f.*
        ys.zip(ysHat).map { case (y, yHat) => 
            (y - yHat) * (y - yHat)
        }.sum / fromInt(ys.size * 2)

    // Sampling
    val rng = new scala.util.Random()
    import Util.*
    val mala = MALA(rng)
    import mala.*

    // running the chain
    val initialSample = Vector(0.0, 0.0, 0.0, 0.0, 0.0)

    val stepSize = 1e-5

    // compute mean and covariance of the samples
    def ewOp(op: (Double, Double) => Double)(a: Vector[Double], b: Vector[Double]): Vector[Double] = 
        a.zip(b).map(op(_, _))
    
    def mean(samples: Seq[Sample]) = samples.reduce(ewOp(_ + _)).map(_ / samples.size.toDouble)
    def cov(samples: Seq[Sample], mean: Sample) = samples.map(s => ewOp(_ - _)(s, mean).map(x => x * x)).reduce(ewOp(_ + _)).map(_ / samples.size.toDouble)

    def predictWith(weights: Sample): Vector[Double] = 
        StandardScaler.inverseScaleColumn(xs_ss.map(x => predict(x, weights)), ys_mean, ys_std)

    {
        println("MALA")

        val dPosterior = ScalaGrad.derive(posterior)
        val q = langevinDynamicsProposal(dPosterior, stepSize, 1.0)
        val g = gDef(dPosterior, stepSize)
        val samples = hastingsSample(posteriorDouble, q, g)(initialSample)
            .drop(50_000)
            .take(10_000).toSeq

        val hastingsMean = mean(samples)

        println(f"hastingsMean = ${hastingsMean}")
        println(f"hastingsCov = ${cov(samples, hastingsMean)}")

        val losses = samples.map(predictWith(_)).map(yHat => Math.sqrt(loss(y, yHat)))
        val meanLoss = losses.reduce(_ + _) / losses.size.toDouble
        val covLoss = losses.map(x => (x - meanLoss) * (x - meanLoss)).reduce(_ + _) / losses.size.toDouble
        println(f"${meanLoss}g +-${covLoss} -- RMSE with learned weights")
    }
    {
        println("Metropolis Hastings")

        val samples = metropolisSampler(rng)(posteriorDouble, gaussianProposal(stepSize))(initialSample)
            .drop(50_000)
            .take(10_000).toSeq
            
        val metropolisMean = mean(samples)
        val metropolisCov = cov(samples, metropolisMean)

        println(f"metropolisMean = ${metropolisMean}")
        println(f"metropolisCov = ${metropolisCov}")

        val losses = samples.map(predictWith(_)).map(yHat => Math.sqrt(loss(y, yHat)))
        val meanLoss = losses.reduce(_ + _) / losses.size.toDouble
        val covLoss = losses.map(x => (x - meanLoss) * (x - meanLoss)).reduce(_ + _) / losses.size.toDouble
        println(f"${meanLoss}g +-${covLoss} -- RMSE with learned weights")
    }


    

package scalagrad.showcase.probabilisticProgramming

import breeze.stats.distributions.Gaussian
import breeze.stats.distributions.Rand.FixedSeed.randBasis
import scala.meta.Member.Param
import spire.math.Numeric
import spire.implicits._
import spire.algebra.Trig
import spire.compat.numeric

import scalagrad.auto.forward.dual.DualNumber
import scalagrad.auto.forward.dual.DualNumber.given
import scalagrad.spire.auto.dual.DualIsNumeric.given
import scalagrad.spire.auto.dual.DualIsNumeric
import scalagrad.auto.forward.DeriverForwardPlan
import scalagrad.auto.forward.DeriverForwardPlan.given

import scalagrad.api.ScalaGrad
import scalagrad.showcase.probabilisticProgramming.distribution.{UnnormalizedDistribution, UnnormalizedLogDistribution}
import scalagrad.showcase.probabilisticProgramming.metropolisHastings.GaussianMetropolisSampler
import scalagrad.showcase.probabilisticProgramming.metropolisHastings.MetropolisAdjustedLangevinAlgorithmSampler
import scala.util.Random

import scaltair.*
import scaltair.PlotTargetBrowser.given

import breeze.stats.meanAndVariance

object UseCase1b extends App:
    val numWarmup = 5_000
    val numSamples = 100
    val numDataPoints = 500
    val a = Vector(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0)  // coefficients for each feature
    val b = 2
    val sigma = 0.5
    val errorDist = Gaussian(0, sigma)

    val data = for (i <- 0 until numDataPoints) yield {
        val xs = Vector.fill(a.size)(Random.nextDouble() * 2 - 1)
        val y = xs.zip(a).map(_ * _).sum + b + errorDist.draw()
        (xs, y)
    }

    case class Parameters[T](a : Vector[T], b:  T, sigma : T)
    object Parameters:
        extension [T] (p: Parameters[T])
            def toVector: Vector[T] = p.a ++ Vector(p.b, p.sigma)
        def fromVector[T](v: Vector[T]): Parameters[T] = Parameters[T](v.slice(0, 10), v(10), v(11))

    def gaussianLogPdf[T: Numeric: Trig](mean: T, sigma: T)(x: T): T = {
        val num = summon[Numeric[T]]
        val trig = summon[Trig[T]]
        val pi = num.fromDouble(math.Pi)
        val two = num.fromInt(2)
        val sigma2 = sigma * sigma
        val diff = x - mean
        val diff2 = diff * diff
        -trig.log(two * pi * sigma2) / 2 - diff2 / (two * sigma2)
    } ensuring { res =>
        def ~=(x: Double, y: Double) = {
            if ((x - y).abs < 0.001) true else false
        }
        val num = summon[Numeric[T]]
        true || ~=(res.toDouble, Gaussian(mean.toDouble, sigma.toDouble).logPdf(x.toDouble))
    }

    def logNormalLogPdf[T: Numeric: Trig](mean: T, sigma: T)(x: T): T =
        val num = summon[Numeric[T]]
        val trig = summon[Trig[T]]
        val pi = num.fromDouble(math.Pi)
        gaussianLogPdf(mean, sigma)(x)

    def logLikelihood[T: Numeric: Trig](p: Parameters[T]): T =
        val num = summon[Numeric[T]]
        val trig = summon[Trig[T]]
        val twoPi = num.fromDouble(2.0) * num.fromDouble(scala.math.Pi)
        
        data.map { case (x, y) =>
            val mu = (p.a zip x).map { case (a, x) => a * num.fromDouble(x) }.sum + p.b
            val diff = num.fromDouble(y) - mu
            val exponent = -num.fromDouble(0.5) * (diff * diff) / (p.sigma * p.sigma)
            val normalization = num.fromDouble(1.0) / (sqrt(twoPi) * p.sigma)
            trig.log(normalization) + exponent
        }.sum

    def logPriorDistA[T: Numeric: Trig](x: T) = 
        val num = summon[Numeric[T]]
        gaussianLogPdf[T](num.zero, num.fromInt(1))(x)
    def logPriorDistB[T: Numeric: Trig](x: T) = 
        val num = summon[Numeric[T]]
        gaussianLogPdf[T](num.zero, num.fromInt(10))(x)
    def logPriorDistSigma[T: Numeric: Trig](x: T) = 
        val num = summon[Numeric[T]]
        val trig = summon[Trig[T]]
        logNormalLogPdf[T](num.zero, num.fromDouble(0.25))(trig.log(x))

    def logPrior[T: Numeric: Trig](p: Parameters[T]): T =
        val num = summon[Numeric[T]]
        val trig = summon[Trig[T]]
        val aPrior = p.a.map(logPriorDistA(_)).sum
        val bPrior = logPriorDistB(p.b)
        val sigmaPrior = logPriorDistSigma(p.sigma)
        aPrior + bPrior + sigmaPrior

    def logPosterior[T: Numeric: Trig](v: Vector[T]): T =
        val p = Parameters.fromVector(v)
        logLikelihood(p) + logPrior(p)

    val initialSample = Parameters(Vector.fill(10)(0.0), 0.0, 1.0).toVector

    println(f"Initial Gradient: ${ScalaGrad.derive(logPosterior[DualNumber[Double]])(initialSample)}")

    lazy val metroSamples =
        GaussianMetropolisSampler(
            new Random(),
            stepSize = 1.0 / numDataPoints, 
        )
            .apply(UnnormalizedLogDistribution(logPosterior[Double]), initialSample)
            .drop(numWarmup)
            .take(numSamples).toSeq

    lazy val malaSamples = 
        MetropolisAdjustedLangevinAlgorithmSampler(
            new Random(),
            ScalaGrad.derive(logPosterior[DualNumber[Double]]),
            stepSize = 1e-2 / numDataPoints, 
            sigma = 1.0
        )
            .apply(UnnormalizedLogDistribution(logPosterior[Double]), initialSample)
            .drop(numWarmup)
            .take(numSamples).toSeq

    def plotSamples(samples: Seq[Parameters[Double]], title: String): Unit =
        // Samples to long format
        val longDf = samples.zipWithIndex.flatMap { case (sample, i) =>
            val a = sample.a.zipWithIndex.map { case (a, j) => s"a$j" -> a }
            val b = Map("b" -> sample.b)
            val sigma = Map("sigma" -> sample.sigma)
            a ++ b ++ sigma
        }
        
        // Stupid way to get the true values in the same format to combine them into the same plot (it ain't stupid if it works, right?)
        val data = Map(
            "key" -> longDf.map(_._1),
            "keyTrue" -> List.fill(samples.size)((0 until 10).map(i => s"a${i}").toList ++ List("b", "sigma")).flatten,
            "value" -> longDf.map(_._2),
            "valueTrue" -> List.fill(samples.size)(a.toList ++ List(b, sigma)).flatten,
            "type" -> longDf.map(_ => "sample"),
            "typeTrue" -> longDf.map(_ => "true"),
        )

        val boxPlot = Chart(data)
            .encode(
                Channel.X("value", FieldType.Quantitative),
                Channel.Y("key", FieldType.Nominal),
                Channel.Color("type"),
            )
            .markBoxplot()

        val trueValues = Chart(data)
            .encode(
                Channel.X("valueTrue", FieldType.Quantitative),
                Channel.Y("keyTrue", FieldType.Nominal),
                Channel.Color("typeTrue"),
            )
            .markCircle()
            
        boxPlot.overlay(trueValues)
            .properties(
                ChartProperties(title=s"$title ${samples.size} samples with $numWarmup warmup and $numDataPoints data points"),
            )
            .show()

    println("Metro")
    plotSamples(
        metroSamples.map(Parameters.fromVector(_)),
        title="Metro"
    )
    
    println("MALA")
    for (i <- malaSamples.size / 10 to malaSamples.size by malaSamples.size / 10) {
        plotSamples(
            malaSamples.take(i).map(Parameters.fromVector(_)),
            title="MALA"
        )
    }

    println("DONE")



    
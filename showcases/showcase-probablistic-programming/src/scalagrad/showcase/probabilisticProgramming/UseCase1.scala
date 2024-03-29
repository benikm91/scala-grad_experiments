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

import breeze.stats.meanAndVariance

object UseCase1 extends App:
    val a = 0.2
    val b = 3
    val sigma = 0.5
    val errorDist = Gaussian(0, sigma)
    val data = for (x <- 0 until 100) yield {
        (x, a * x + b + errorDist.draw())
    }

    case class Parameters[T](a : T, b:  T, sigma : T)
    object Parameters:
        extension [T] (p: Parameters[T])
            def toVector: Vector[T] = Vector(p.a, p.b, p.sigma)
        def fromVector[T](v: Vector[T]): Parameters[T] = Parameters[T](v(0), v(1), v(2))

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
        // println(f"${mean.toDouble}, ${sigma.toDouble}")
        // println(f"${res.toDouble} ~= ${Gaussian(mean.toDouble, sigma.toDouble).logPdf(x.toDouble)}")
        true || ~=(res.toDouble, Gaussian(mean.toDouble, sigma.toDouble).logPdf(x.toDouble))
    }

    def logNormalLogPdf[T: Numeric: Trig](mean: T, sigma: T)(x: T): T =
        val num = summon[Numeric[T]]
        val trig = summon[Trig[T]]
        val pi = num.fromDouble(math.Pi)
        gaussianLogPdf(mean, sigma)(x)

    def logLikelihood[T: Numeric: Trig](p: Parameters[T]): T =
        val num = summon[Numeric[T]]
        val likelihoods = 
            for ((x, y) <- data) yield
                gaussianLogPdf(p.a * num.fromDouble(x) + p.b, p.sigma)(num.fromDouble(y))
        likelihoods.sum

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
        logPriorDistA(p.a) + logPriorDistB(p.b) + logPriorDistSigma(p.sigma)

    def logPosterior[T: Numeric: Trig](v: Vector[T]): T =
        val p = Parameters.fromVector(v)
        logLikelihood(p) + logPrior(p)

    val stepSize = 0.1
    val initialSample = Parameters(0.0, 0.0, 1.0).toVector

    println(f"Initial Gradient: ${ScalaGrad.derive(logPosterior[DualNumber[Double]])(initialSample)}")

    lazy val metroSamples =
        GaussianMetropolisSampler(
            new Random(),
            stepSize, 
        )
            .apply(UnnormalizedLogDistribution(logPosterior[Double]), initialSample)
            .drop(5_000)
            .take(10_000).toSeq

    lazy val malaSamples = 
        MetropolisAdjustedLangevinAlgorithmSampler(
            new Random(),
            ScalaGrad.derive(logPosterior[DualNumber[Double]]),
            stepSize = 1e-6,
            sigma = 1.0
        )
            .apply(UnnormalizedLogDistribution(logPosterior[Double]), initialSample)
            .drop(50_000)
            .take(50_000).toSeq

    {
        println("Metro")
        val samples = metroSamples.map(Parameters.fromVector(_))

        val meanAndVarianceA = meanAndVariance(samples.map(_.a))
        println(s"Estimates for parameter a: mean = ${meanAndVarianceA.mean}, var = ${meanAndVarianceA.variance}")
        val meanAndVarianceB = meanAndVariance(samples.map(_.b))
        println(s"Estimates for parameter b: mean = ${meanAndVarianceB.mean}, var = ${meanAndVarianceB.variance}")
        val meanAndVariancesigma = meanAndVariance(samples.map(_.sigma))
        println(s"Estimates for parameter sigma: mean = ${meanAndVariancesigma.mean}, var = ${meanAndVariancesigma.variance}")
        println("***")
    }
    {
        val samples = malaSamples.map(Parameters.fromVector(_))

        println("MALA")
        val meanAndVarianceA = meanAndVariance(samples.map(_.a))
        println(s"Estimates for parameter a: mean = ${meanAndVarianceA.mean}, var = ${meanAndVarianceA.variance}")
        val meanAndVarianceB = meanAndVariance(samples.map(_.b))
        println(s"Estimates for parameter b: mean = ${meanAndVarianceB.mean}, var = ${meanAndVarianceB.variance}")
        val meanAndVariancesigma = meanAndVariance(samples.map(_.sigma))
        println(s"Estimates for parameter sigma: mean = ${meanAndVariancesigma.mean}, var = ${meanAndVariancesigma.variance}")
        println("***")
        
        println(f"End Gradient: ${ScalaGrad.derive(logPosterior[DualNumber[Double]])(Vector(meanAndVarianceA.mean, meanAndVarianceB.mean, meanAndVariancesigma.mean))}")

    }



    
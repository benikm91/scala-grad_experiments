//> using scala "3.2"
//> using repository "sonatype:snapshots"
//> using dep "ch.unibas.cs.gravis::scalismo-ui:0.91.2"
//> using dep "ch.unibas.cs.gravis::scalismo-plot:0.3-SNAPSHOT"

package scalagrad.showcase.probabilisticProgramming

import scalagrad.auto.forward.dual.DualNumber
import scalagrad.auto.forward.dual.DualNumber.given
import scalagrad.spire.auto.dual.DualIsNumeric.given
import scalagrad.spire.auto.dual.DualIsNumeric
import scalagrad.auto.forward.DeriverForwardPlan
import scalagrad.auto.forward.DeriverForwardPlan.given

import spire.math.Numeric
import spire.algebra.Trig
import spire.implicits.*
import spire.compat.numeric
import scala.util.Random

import scalagrad.api.ScalaGrad
import scalagrad.showcase.probabilisticProgramming.distribution.{UnnormalizedDistribution, UnnormalizedLogDistribution}
import scalagrad.showcase.probabilisticProgramming.metropolisHastings.GaussianMetropolisSampler
import scalagrad.showcase.probabilisticProgramming.metropolisHastings.MetropolisAdjustedLangevinAlgorithmSampler

import scaltair.*
import scaltair.PlotTargetBrowser.given

object MHSampling extends App {

    import breeze.linalg.{Vector => _, *}
    import breeze.linalg.InjectNumericOps
    import breeze.stats.distributions.Rand.FixedSeed.randBasis
    import breeze.stats.distributions.Gaussian
    import breeze.stats.distributions.MultivariateGaussian
    import breeze.numerics.step
    import breeze.numerics.exp

    val rng = new Random()
    val numWarmup = 10_000
    val numSamples = 12_000
 
    // target distribution from which we want to sample
    def pLog[T: Numeric: Trig](x: Vector[T], mean: Vector[T], cov: Vector[Vector[T]]): T = {
        val trig = summon[Trig[T]]
        Stuff.logMultivariateGaussianPdf(x, mean, cov)
    }

    // target distribution from which we want to sample
    def p[T: Numeric: Trig](x: Vector[T], mean: Vector[T], cov: Vector[Vector[T]]): T = {
        val trig = summon[Trig[T]]
        Stuff.multivariateGaussianPdf(x, mean, cov)
    }

    val stepSize = 1.0
    val initialSample = Vector(0.0, 0.0)

    val mean = Vector(3.0, 1.0)
    val covariance = Vector(
        Vector(50.0, 45.0), 
        Vector(45.0, 50.0)
    )
    
    def pDouble(x: Vector[Double]): Double = p[Double](x, mean, covariance)
    def pLogDouble(x: Vector[Double]): Double = pLog[Double](x, mean, covariance)

    val meanDN = mean.map(DualNumber(_, 0.0))
    val covarianceDN = covariance.map(_.map(DualNumber(_, 0.0)))
    
    def pDualNumber(x: Vector[DualNumber[Double]]): DualNumber[Double] = p[DualNumber[Double]](x, meanDN, covarianceDN)
    def pLogDualNumber(x: Vector[DualNumber[Double]]): DualNumber[Double] = pLog[DualNumber[Double]](x, meanDN, covarianceDN)

    // val (target, dTarget) = (UnnormalizedDistribution(pDouble), ScalaGrad.derive(pDualNumber))
    val (target, dTarget) = (UnnormalizedLogDistribution(pLogDouble), ScalaGrad.derive(pLogDualNumber))

    // TODO is here really log?
    lazy val metro =
        GaussianMetropolisSampler(
            new Random(),
            stepSize, 
        )
    lazy val metroSamples = metro
            .apply(target, initialSample)
            .drop(numWarmup)
            .take(numSamples).toSeq

    lazy val mala = 
        MetropolisAdjustedLangevinAlgorithmSampler(
            new Random(),
            dTarget,
            stepSize,
            sigma = 1.0
        )
    lazy val malaSamples = mala
            .apply(target, initialSample)
            .drop(numWarmup)
            .take(numSamples).toSeq

    lazy val bMetroSamples = metroSamples.map(s => DenseVector(s.toArray))

    lazy val metroMean = bMetroSamples.reduce(_ + _) / bMetroSamples.size.toDouble
    lazy val metroCov = bMetroSamples.map(s => (s - metroMean) * (s - metroMean).t).reduce(_ + _) / bMetroSamples.size.toDouble

    def plotSamples(samples: Seq[Seq[Double]], title: String) =
        val data = Data.fromRows(samples.map(v => Map("x1" -> v(0), "x2" -> v(1))))
        val plot = Chart(data)
            .encode(
                Channel.X("x1", FieldType.Quantitative),
                Channel.Y("x2", FieldType.Quantitative),
            )
            .markCircle()
            .properties(
                ChartProperties(
                    title=s"$title $numSamples samples with $numWarmup warmup"
                )
            )
            .show()

    plotSamples(metroSamples, f"Metro (${metro.showHyperParams})")

    println(f"metroMean ${metroMean}")
    println(f"metroCov ${metroCov}")

    lazy val bMalaSamples = malaSamples.map(s => DenseVector(s.toArray))

    lazy val malaMean = bMalaSamples.reduce(_ + _) / bMalaSamples.size.toDouble
    lazy val malaCov = bMalaSamples.map(s => (s - malaMean) * (s - malaMean).t).reduce(_ + _) / bMalaSamples.size.toDouble

    plotSamples(malaSamples, f"MALA (${mala.showHyperParams})")

    println(f"malaMean ${malaMean}")
    println(f"malaCov ${malaCov}")

}

/**
 * Stuff that is in Scalismo but I need to implement it here abstractly with Spire to make it work with ScalaGrad.
 * 
 * Code mostly generated with ChatGPT.
 * 
 * Scalismo Implementation:
    def next(current: A, logger: AcceptRejectLogger[A]): A = {
    // reference p value
    val currentP = evaluator.logValue(current)
    // proposal
    val proposal = generator.propose(current)
    val proposalP = evaluator.logValue(proposal)
    // transition ratio
    val t = generator.logTransitionRatio(current, proposal)
    // acceptance probability
    val a = proposalP - currentP - t

    // accept or reject
    if (a > 0.0 || random.scalaRandom.nextDouble() < exp(a)) {
      logger.accept(current, proposal, generator, evaluator)
      proposal
    } else {
      logger.reject(current, proposal, generator, evaluator)
      current
    }
  }
 * 
 */
object Stuff:
    import spire.math.Numeric
    import spire.algebra.Trig
    import spire.implicits._
    import scala.reflect.ClassTag

    def logMultivariateGaussianPdf[T: Numeric: Trig](x: Vector[T], mean: Vector[T], covariance: Vector[Vector[T]]): T = {
        // todo implement log Multivariate Gaussian PDF with spire (numerical stable)
        val trig = summon[Trig[T]]
        val num = summon[Numeric[T]]
        val n = x.length
        val det = determinant(covariance)
        val invCovariance = inverse(covariance)
        val diff = subtractVectors(x, mean)
        val exponent = num.fromDouble(-0.5) * dotProduct(diff, matrixVectorProduct(invCovariance, diff))
        val coefficient = num.fromDouble(math.pow(2 * math.Pi, n)) * det
        num.fromDouble(-0.5) * trig.log(coefficient) + exponent
    }

    def multivariateGaussianPdf[T: Numeric: Trig](x: Vector[T], mean: Vector[T], covariance: Vector[Vector[T]]): T = {
        val trig = summon[Trig[T]]
        val num = summon[Numeric[T]]
        val n = x.length
        val det = determinant(covariance)
        val invCovariance = inverse(covariance)
        val diff = subtractVectors(x, mean)
        val exponent = num.fromDouble(-0.5) * dotProduct(diff, matrixVectorProduct(invCovariance, diff))
        val coefficient = num.one / (num.fromDouble(math.pow(2 * math.Pi, n / 2.0)) * num.sqrt(det))
        coefficient * trig.exp(exponent)
    }

    def determinant[T: Numeric](matrix: Vector[Vector[T]]): T = {
    val num = summon[Numeric[T]]
    val n = matrix.length
    if (n == 1) matrix(0)(0)
    else {
        var det = num.zero
        var sign = num.one
        for (i <- 0 until n) {
            val subMatrix = subMatrixWithoutRowAndColumn(matrix, i, 0)
            det += sign * matrix(0)(i) * determinant(subMatrix)
            sign *= -1
        }
        det
    }
    }

    def subMatrixWithoutRowAndColumn[T: Numeric](matrix: Vector[Vector[T]], row: Int, col: Int): Vector[Vector[T]] = {
        matrix.zipWithIndex.filterNot(_._2 == row).map(x => x._1.take(col) ++ x._1.drop(col + 1))
    }

    def inverse[T: Numeric](matrix: Vector[Vector[T]]): Vector[Vector[T]] = {
        val det = determinant(matrix)
        val adjugate = transpose(cofactorMatrix(matrix))
        adjugate.map(_.map(_ / det))
    }

    def transpose[T: Numeric](matrix: Vector[Vector[T]]): Vector[Vector[T]] = {
        matrix.head.indices.map(col => matrix.map(row => row(col))).toVector
    }

    def cofactorMatrix[T: Numeric](matrix: Vector[Vector[T]]): Vector[Vector[T]] = {
        val num = summon[Numeric[T]]
        matrix.indices.map(i =>
            matrix(i)
                .indices
                .map(j => determinant(subMatrixWithoutRowAndColumn(matrix, i, j)) * num.fromInt(math.pow(-1, i + j).toInt))
                .toVector
        ).toVector
    }

    def subtractVectors[T: Numeric](a: Vector[T], b: Vector[T]): Vector[T] = {
        a.zip(b).map { case (ai, bi) => ai - bi }
    }

    def dotProduct[T: Numeric](a: Vector[T], b: Vector[T]): T = {
        a.zip(b).map { case (ai, bi) => ai * bi }.sum
    }

    def matrixVectorProduct[T: Numeric](matrix: Vector[Vector[T]], vector: Vector[T]): Vector[T] = {
        matrix.map(row => dotProduct(row, vector))
    }
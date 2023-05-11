package scalagrad.showcase.probabilisticProgramming

import breeze.stats.distributions.MultivariateGaussian
import scala.math.{sqrt, exp, min}
import breeze.linalg.{DenseVector, DenseMatrix}
import scala.util.Random
import breeze.stats.distributions.Rand.FixedSeed.randBasis

object Util:

    type Sample = Vector[Double]
    type Proposal = Sample => Sample 
    type Distribution = Sample => Double 
    type ConditionalDistribution = (Sample, Sample) => Double

    def gaussianProposal(stepSize: Double)(x : Sample): Sample = {
        val mvn = MultivariateGaussian(
            DenseVector(x.toArray),
            DenseMatrix.eye[Double](x.size) * stepSize,
        )
        mvn.draw().toArray.toVector
    }

    // q suggests a new sample given the current sample
    // p is the target distribution from which we want to sample
    def metropolisSampler(rng: Random)(p : Distribution, q : Proposal, log: Boolean = true)(initialSample : Sample) : Iterator[Sample] = {

        def nextStep(currentSample : Sample) : Sample = {
            val proposedSample = q(currentSample)

            val acceptanceProp = 
                if log then
                    exp(p(proposedSample) - p(currentSample))
                else 
                    p(proposedSample) / p(currentSample)

            val alpha = min(1.0, acceptanceProp)
            val r = rng.nextDouble()
            if (r < alpha) then proposedSample else currentSample
        }


        Iterator.iterate(initialSample)(nextStep)
    }


case class MALA(rng: Random):

    import Util.*

    def hastingsSample(p: Distribution, q: Proposal, g: ConditionalDistribution, log: Boolean = true)(initialSample : Sample) : Iterator[Sample] = {
        
        def nextStep(currentSample : Sample) : Sample = {
            val proposedSample = q(currentSample)

            val acceptanceProp = 
                if log then
                    val pp = p(proposedSample) - p(currentSample)
                    val gg = g(currentSample, proposedSample) - g(proposedSample, currentSample)

                    exp(pp + gg)
                else 
                    val pp = p(proposedSample) / p(currentSample)
                    val gg = g(currentSample, proposedSample) - g(proposedSample, currentSample)
                    pp * gg                    

            val alpha = min(1.0, acceptanceProp)
            
            val r = rng.nextDouble()
            if (r < alpha) then proposedSample else currentSample
        }

        Iterator.iterate(initialSample)(nextStep)
    }

    def gaussianDiagSample(x : Sample, sigma: Double): Sample = {
        val mvn = MultivariateGaussian(
            DenseVector(x.toArray),
            DenseMatrix.eye[Double](x.size) * sigma,
        )
        mvn.draw().toArray.toVector
    }

    def gDef(dp: Vector[Double] => Vector[Double], stepSize: Double)(x: Sample, cx: Sample): Double = {
        val step = dp(x).map(_ * stepSize)
        val v = x.zip(cx).map(_ - _).zip(step).map(_ - _)
        val vsq = v.zip(v).map(_ * _).reduce(_ + _)
        -0.25 * stepSize * vsq
    }

    def langevinDynamicsProposal(dp: Vector[Double] => Vector[Double], stepSize: Double, sigma: Double)(x: Sample) = {
        val step = dp(x).map(_ * stepSize)
        val gaussianNoise = gaussianDiagSample(Vector.fill(x.size)(0), 1.0)
        val noiseF = sqrt(2 * stepSize)
        val noise = gaussianNoise.map(_ * noiseF)
        x.zip(step).map(_ + _).zip(noise).map(_ + _)
    }
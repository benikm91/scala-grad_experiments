package scalagrad.showcase.deeplearning

import scalagrad.api.ScalaGrad
import scalagrad.api.Dual
import scalagrad.api.VectorAlgebraFor
import scalagrad.api.VectorAlgebraOps
import scalagrad.auto.forward.dual.DualNumber
import scalagrad.fractional.auto.dual.DualIsFractional.given
import scalagrad.auto.reverse.dual.DualDelta

class VectorAlgebraForDualNumber[P: Fractional]() extends VectorAlgebraFor[DualNumber[P]]()

import breeze.linalg.{Vector => _, *}
import breeze.linalg.operators._
import breeze.linalg.support._
import scalagrad.api.Deriver
import scalagrad.api.DeriverPlan
import scala.reflect.ClassTag
import scala.math.Fractional
import scala.runtime.Tuples

import scalagrad.auto.forward.dual.DualNumber


def useIt(ops: VectorAlgebraOps)(v: ops.ColumnVector, m: ops.Matrix) =
    import ops.*
    m * v

def useIt2(ops: VectorAlgebraOps)(v: ops.ColumnVector): ops.Scalar =
    import ops.*
    transposeColumVector(v) * v

def scenario2(ops: VectorAlgebraOps)(v: ops.ColumnVector): ops.Scalar =
    import ops.*
    val m = v * transposeColumVector(v)
    val v2 = m * v
    transposeColumVector(v2) * v2

@main
def test() =
    println(useIt(VectorAlgebraFor[Double]())(Vector(1.0, 2.0), Vector(Vector(1.0, 2.0), Vector(3.0, 4.0))))
    println(useIt(BreezeVectorAlgebraForDouble)(DenseVector(1.0, 2.0), DenseMatrix((1.0, 2.0), (3.0, 4.0))))
    println(useIt2(VectorAlgebraFor[Double]())(Vector(1.0, 2.0)))
    println(useIt2(BreezeVectorAlgebraForDouble)(DenseVector(1.0, 2.0)))
    
    {
        import scalagrad.auto.forward.DeriverForwardPlan.given
        val df = ScalaGrad.derive(useIt2(VectorAlgebraForDualNumber[Double]()))
        println(df(Vector(1.0, 2.0)))
    }
    {
        import DeriverBreezeForwardPlan.given
        val f = useIt2(BreezeVectorAlgebraForDualNumberDouble)
        val df = ScalaGrad.derive(f)
        println(df(DenseVector(1.0, 2.0)))
    }
    {
        import DeriverBreezeReversePlan.given
        val f = useIt2(BreezeVectorAlgebraForDualDeltaDouble)
        val df = ScalaGrad.derive(f)
        println(df(DenseVector(1.0, 2.0)))
    }
    {
        import DeriverBreezeForwardPlan.given
        val f = scenario2(BreezeVectorAlgebraForDualNumberDouble)
        val df = ScalaGrad.derive(f)
        println(df(DenseVector(1.0, 2.0)))
    }
    {
        import DeriverBreezeReversePlan.given
        val f = scenario2(BreezeVectorAlgebraForDualDeltaDouble)
        val df = ScalaGrad.derive(f)
        println(df(DenseVector(1.0, 2.0)))
    }

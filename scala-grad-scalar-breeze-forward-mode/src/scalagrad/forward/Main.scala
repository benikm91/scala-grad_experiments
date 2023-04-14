package scalagrad.forward

import scalagrad.api.ScalaGrad
import scalagrad.api.Deriver
import breeze.linalg.DenseVector
import breeze.linalg.sum
import breeze.linalg.operators.DenseVectorExpandOps
import breeze.math.Field
import breeze.linalg.support.CanTraverseValues
import breeze.math.Ring
import scalagrad.forward.api.DeriverBreeze
import breeze.math.Semiring
import breeze.linalg.product

object Main {
    def main(args: Array[String]): Unit = {
        val a = summon[CanTraverseValues[DenseVector[DualNumber[Double]], DualNumber[Double]]]
        
        import DeriverBreeze.given
        import DeriverBreezeForward.given

        def ff(xs: DenseVector[DeriverForward.DNum[Double]]): DeriverForward.DNum[Double] =
            sum(xs)
        
        def f1[T: Field](xs: DenseVector[T]): T = 
            import breeze.linalg.InjectNumericOps
            xs.reduce(_ * _)
        
        val dff = ScalaGrad.derive(ff)
        println(dff(DenseVector(1.0, 2.0, 3.0)))
        val df1 = ScalaGrad.derive(f1[DualNumber[Double]])
        println(df1(DenseVector(1.0, 2.0, 3.0)))
    }
}
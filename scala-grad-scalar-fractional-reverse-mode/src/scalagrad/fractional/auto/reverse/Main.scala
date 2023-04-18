package scalagrad.fractional.auto.reverse

import scalagrad.api.ScalaGrad
import scalagrad.api.Deriver

import scalagrad.auto.reverse.DeriverReversePlan.given
import scalagrad.auto.reverse.dual.DualDelta
import scalagrad.fractional.auto.reverse.dual.DualDeltaIsFractional.given

object Main {
    def main(args: Array[String]): Unit = {

        // Example Scalar
        def f1[T](x: T)(using f: Fractional[T]): T = 
            import f._
            x * x
        val df1 = ScalaGrad.derive(f1[DualDelta[Double]])
        println(df1(1.0))

        // Example 2 Scalar
        def f2[T](x1: T, x2: T)(using f: Fractional[T]): T = 
            import f._
            x1 * x2
        val df2 = ScalaGrad.derive(f2[DualDelta[Double]])
        println(df2(1.0, 2.0))

        // Example Vector
        def f3[T](xs: Vector[T])(using f: Fractional[T]): T = 
            import f._
            xs.reduce(_ * _)
        val df3 = ScalaGrad.derive(f3[DualDelta[Double]])
        println(df3(Vector(1.0, 2.0, 3.0)))

    }
}

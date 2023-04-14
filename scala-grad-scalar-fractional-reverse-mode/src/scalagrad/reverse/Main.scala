package scalagrad.reverse

import scalagrad.api.ScalaGrad
import scalagrad.api.Deriver

object Main {
    def main(args: Array[String]): Unit = {

        // Example Scalar
        def f1[T](x: T)(using f: Fractional[T]): T = 
            import f._
            x * x
        import DeriverReverse.given
        val df1 = ScalaGrad.derive(f1[DeriverReverse.DNum[Double]])
        println(df1(1.0))

        // Example 2 Scalar
        def f2[T](x1: T, x2: T)(using f: Fractional[T]): T = 
            import f._
            x1 * x2
        import DeriverReverse.given
        val df2 = ScalaGrad.derive(f2[DeriverReverse.DNum[Double]])
        println(df2(1.0, 2.0))

        // Example Vector
        def f3[T](xs: Vector[T])(using f: Fractional[T]): T = 
            import f._
            xs.reduce(_ * _)
        val df3 = ScalaGrad.derive(f3[DeriverReverse.DNum[Double]])
        println(df3(Vector(1.0, 2.0, 3.0)))

    }
}

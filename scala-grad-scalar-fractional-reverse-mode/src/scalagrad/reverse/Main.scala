package scalagrad.reverse

import scalagrad.api.ScalaGrad
import scalagrad.api.Deriver

object Main {
    def main(args: Array[String]): Unit = {
        def f1[T](xs: Array[T])(using f: Fractional[T]): T = 
            import f._
            xs.reduce(_ * _)
        def f2[T](x1: T, x2: T)(using f: Fractional[T]): T = 
            import f._
            x1 * x2
        import DeriverReverse.given
        val df2 = ScalaGrad.derive(f2[DeriverReverse.DNum[Double]])
        println(df2(1.0, 2.0))
        // val df1 = ScalaGrad.derive(f1[DeriverReverse.DNum[Double]])
        // println(df1(Array(1.0, 2.0, 3.0)).mkString(", "))
    }
}

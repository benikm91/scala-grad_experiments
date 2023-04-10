package scalagrad.forward

import scalagrad.api.ScalaGrad
import scalagrad.api.Deriver

object Main {
    def main(args: Array[String]): Unit = {
        def f1[T](xs: Array[T])(using f: Fractional[T]): T = 
            import f._
            xs.reduce(_ * _)
        def f2[T](x1: T, x2: T, x3: T)(using f: Fractional[T]): T = 
            import f._
            x1 * x2 * x3
        import DeriverForward.given
        val df1 = ScalaGrad.derive(f1[DeriverForward.DNum[Double]])
        println(df1(Array(1.0, 2.0, 3.0)).mkString(", "))
    }
}

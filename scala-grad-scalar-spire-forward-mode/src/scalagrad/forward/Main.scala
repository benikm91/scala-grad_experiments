package scalagrad.forward

import scalagrad.api.ScalaGrad
import scalagrad.api.Deriver
import spire.math.Numeric
import spire.implicits._

object Main {
    def main(args: Array[String]): Unit = {
        import DeriverForward.given

        def f1[T: Numeric](x: T): T = 
            x * x
        val df1 = ScalaGrad.derive(f1[DeriverForward.DNum[Double]])
        println(df1(1.0))

        // Example 2 Scalar
        def f2[T: Numeric](x1: T, x2: T): T = 
            x1 * x2
        val df2 = ScalaGrad.derive(f2[DeriverForward.DNum[Double]])
        println(df2(1.0, 2.0))

        def f3[T: Numeric](xs: Vector[T]): T = 
            xs.reduce(_ * _)
        val df3 = ScalaGrad.derive(f3[DeriverForward.DNum[Double]])
        println(df3(Vector(1.0, 2.0, 3.0)).mkString(", "))

        def fnegate[T: Numeric](x: T): T = -x
        val dfnegate = ScalaGrad.derive(fnegate[DeriverForward.DNum[Double]])
        println(dfnegate(10.0))

        // def ffpow[T: Numeric](x: T) = 
        //     x.fpow(summon[Numeric[T]].fromDouble(2.0))
        // 
        // val dffpow = ScalaGrad.derive(ffpow[DeriverForward.DNum[Double]])
        // println(dffpow(2.0) + " " + dffpow(4.0))
    }
}

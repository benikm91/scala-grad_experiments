package scalagrad.forward

import scalagrad.api.ScalaGrad
import scalagrad.api.Deriver
import spire.math.Numeric
import spire.implicits._
import scalagrad.forward.dual.DualNumber
import scalagrad.forward.dual.DualNumber.given
import algebra.ring.Ring
import algebra.ring.Field
import spire.algebra.Trig

object Main {
    def main(args: Array[String]): Unit = {
        import DeriverSpireNumericForward.given

        summon[Numeric[DualNumber[Double]]]

        def f1[T: Numeric](x: T): T = 
            x * x
        val df1 = ScalaGrad.derive(f1[DeriverSpireNumericForward.DNum[Double]])
        println(df1(1.0))

        // Example 2 Scalar
        def f2[T: Numeric](x1: T, x2: T): T = 
            x1 * x2
        val df2 = ScalaGrad.derive(f2[DeriverSpireNumericForward.DNum[Double]])
        println(df2(1.0, 2.0))

        def f3[T: Numeric](xs: Vector[T]): T = 
            xs.reduce(_ * _)
        val df3 = ScalaGrad.derive(f3[DeriverSpireNumericForward.DNum[Double]])
        println(df3(Vector(1.0, 2.0, 3.0)).mkString(", "))

        def fnegate[T: Numeric](x: T): T = -x
        val dfnegate = ScalaGrad.derive(fnegate[DeriverSpireNumericForward.DNum[Double]])
        println(dfnegate(10.0))
        
        def fcos[T: Trig](x: T): T = 
            summon[Trig[T]].cos(x)
        val dfcos = ScalaGrad.derive(fcos[DeriverSpireNumericForward.DNum[Double]])
        val pi = summon[Trig[Double]].pi
        println(dfcos(0.0) + " " + dfcos(pi / 2) + " " + dfcos(pi))
        
        def ffpow[T: Numeric](x: T) = 
            x.fpow(summon[Numeric[T]].fromDouble(2.0))
        
        val dffpow = ScalaGrad.derive(ffpow[DeriverSpireNumericForward.DNum[Double]])
        println(dffpow(2.0) + " " + dffpow(4.0))
    }
}

package scalagrad.spire.auto.forward

import scalagrad.api.ScalaGrad
import scalagrad.api.Deriver
import spire.math.Numeric
import spire.implicits._
import scalagrad.auto.forward.dual.DualNumber
import scalagrad.auto.forward.DeriverForwardPlan.given
import algebra.ring.Ring
import algebra.ring.Field
import spire.algebra.Trig

import scalagrad.spire.auto.dual.DualIsNumeric.given
import scalagrad.spire.auto.dual.DualIsNumeric
import scalagrad.auto.forward.DeriverForwardPlan

object Main {
    def main(args: Array[String]): Unit = {

        summon[Numeric[DualNumber[Double]]]

        def f1[T: Numeric](x: T): T = 
            x * x
        val df1 = ScalaGrad.derive(f1[DualNumber[Double]])
        println(df1(1.0))

        // Example 2 Scalar
        def f2[T: Numeric](x1: T, x2: T): T = 
            x1 * x2
        val df2 = ScalaGrad.derive(f2[DualNumber[Double]])
        println(df2(1.0, 2.0))

        def f3[T: Numeric](xs: Vector[T]): T = 
            xs.reduce(_ * _)
        val df3 = ScalaGrad.derive(f3[DualNumber[Double]])
        println(df3(Vector(1.0, 2.0, 3.0)).mkString(", "))

        def fnegate[T: Numeric](x: T): T = -x
        val dfnegate = ScalaGrad.derive(fnegate[DualNumber[Double]])
        println(dfnegate(10.0))
        
        def fcos[T: Trig](x: T): T = 
            summon[Trig[T]].cos(x)
        val dfcos = ScalaGrad.derive(fcos[DualNumber[Double]])
        val pi = summon[Trig[Double]].pi
        println(dfcos(0.0) + " " + dfcos(pi / 2) + " " + dfcos(pi))
        
        def ffpow[T: Numeric](x: T) = 
            x.fpow(summon[Numeric[T]].fromDouble(2.0))
        
        val dffpow = ScalaGrad.derive(ffpow[DualNumber[Double]])
        println(dffpow(2.0) + " " + dffpow(4.0))
    }
}

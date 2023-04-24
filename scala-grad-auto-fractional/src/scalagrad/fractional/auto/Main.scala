package scalagrad.fractional.auto

import scalagrad.api.ScalaGrad
import scalagrad.api.Deriver
import scalagrad.fractional.auto.dual.DualIsFractional.given

import scalagrad.auto.forward.DeriverForwardPlan

object Main {

    def main(args: Array[String]): Unit = {
        println("forwardMode")
        forwardMode()
        println("forwardModeTwice")
        forwardModeTwice()
        println("forwardReverseMode")
        forwardReverseMode()
        println("reverseMode")
        reverseMode()
    }

    def forwardMode(): Unit = {

        import scalagrad.auto.forward.dual.DualNumber
        import scalagrad.auto.forward.dual.DualNumber.given
        import scalagrad.auto.forward.DeriverForwardPlan.given

        def f1[T](x: T)(using f: Fractional[T]): T = 
            import f._
            x * x
        val df1 = ScalaGrad.derive(f1[DualNumber[Double]])
        println(df1(1.0))

        // Example 2 Scalar
        def f2[T](x1: T, x2: T)(using f: Fractional[T]): T = 
            import f._
            x1 * x2
        val df2 = ScalaGrad.derive(f2[DualNumber[Double]])
        println(df2(1.0, 2.0))

        def f3[T](xs: Vector[T])(using f: Fractional[T]): T = 
            import f._
            xs.reduce(_ * _)
        val df3 = ScalaGrad.derive(f3[DualNumber[Double]])
        println(df3(Vector(1.0, 2.0, 3.0)).mkString(", "))

    }

    def forwardModeTwice(): Unit = 
        import scalagrad.auto.forward.dual.DualNumber
        import scalagrad.auto.forward.dual.DualNumber.given
        import scalagrad.auto.forward.DeriverForwardPlan.given

        def f1[T](x: T)(using f: Fractional[T]): T = 
            import f._
            x * x

        def doDeriveForward[P: Fractional](f: [T] => T => Fractional[T] ?=> T): P => P = 
            ScalaGrad.derive(f[DualNumber[P]])
        
        def deriveTwice[P: Fractional](f: [T] => T => Fractional[T] ?=> T) =
            ScalaGrad.derive(ScalaGrad.derive(f1[DualNumber[DualNumber[P]]]))

        val ddf = deriveTwice[Double]([T] => (x: T) => (f: Fractional[T]) ?=> f1(x))
        println(ddf(5.0))

    def forwardReverseMode(): Unit = 
        import scalagrad.auto.forward.dual.DualNumber
        import scalagrad.auto.forward.dual.DualNumber.given
        import scalagrad.auto.reverse.dual.DualDelta
        import scalagrad.auto.reverse.dual.DualDelta.given
        import scalagrad.auto.forward.DeriverForwardPlan.given
        import scalagrad.auto.reverse.DeriverReversePlan.given

        def f1[T](x: T)(using f: Fractional[T]): T = 
            import f._
            x * x

        def doDeriveForward[P: Fractional](f: [T] => T => Fractional[T] ?=> T): P => P = 
            ScalaGrad.derive(f[DualNumber[P]])
        
        def deriveTwice[P: Fractional](f: [T] => T => Fractional[T] ?=> T) =
            ScalaGrad.derive(ScalaGrad.derive(f1[DualNumber[DualDelta[P]]]))

        val ddf = deriveTwice[Double]([T] => (x: T) => (f: Fractional[T]) ?=> f1(x))
        println(ddf(5.0))

    def reverseMode(): Unit = {

        import scalagrad.auto.reverse.dual.DualDelta
        import scalagrad.auto.reverse.dual.DualDelta.given
        import scalagrad.auto.reverse.DeriverReversePlan.given

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

        def f3[T](xs: Vector[T])(using f: Fractional[T]): T = 
            import f._
            xs.reduce(_ * _)
        val df3 = ScalaGrad.derive(f3[DualDelta[Double]])
        println(df3(Vector(1.0, 2.0, 3.0)).mkString(", "))

    }
}

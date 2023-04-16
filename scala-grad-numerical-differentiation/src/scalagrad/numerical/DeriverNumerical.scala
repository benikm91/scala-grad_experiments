package scalagrad.numerical

import scalagrad.api.Deriver
import scala.reflect.ClassTag

object DeriverNumerical:

    given approx1Double: Deriver[Double => Double] with
        val e = 1e-6

        override type dfInput = Double
        override type dfOutput = Double
        override def derive(f: Double => Double): dfInput => dfOutput = (x) =>
            (f(x + e) - f(x)) / e

    given approx2Double: Deriver[(Double, Double) => Double] with
        val e = 1e-6

        override type dfInput = (Double, Double)
        override type dfOutput = (Double, Double)
        override def derive(f: (Double, Double) => Double): dfInput => dfOutput = (x1, x2) => (
            (f(x1 + e, x2) - f(x1, x2)) / e, 
            (f(x1, x2 + e) - f(x1, x2)) / e
        )

    def approx[P](e: P)(using f: Fractional[P]): Deriver[P => P] = new Deriver[P => P] {
        import f.*

        override type dfInput = P
        override type dfOutput = P
        override def derive(f: P => P): dfInput => dfOutput = (x) =>
            (f(x + e) - f(x)) / e
    }

    def approx2[P](e: P)(using f: Fractional[P]): Deriver[(P, P) => P] = new Deriver[(P, P) => P] {
        import f.*

        override type dfInput = (P, P)
        override type dfOutput = (P, P)
        override def derive(f: (P, P) => P): dfInput => dfOutput = (x1, x2) => (
            (f(x1 + e, x2) - f(x1, x2)) / e, 
            (f(x1, x2 + e) - f(x1, x2)) / e
        )
    }

    def approxVector[P](e: P)(using f: Fractional[P]): Deriver[Vector[P] => P] = new Deriver[Vector[P] => P] {
        import f.*

        override type dfInput = Vector[P]
        override type dfOutput = Vector[P]
        override def derive(f: Vector[P] => P): dfInput => dfOutput = (xs) => (
            for (keyX <- xs.indices)
                yield (f(xs.updated(keyX, xs(keyX) + e)) - f(xs)) / e
        ).toVector
    }

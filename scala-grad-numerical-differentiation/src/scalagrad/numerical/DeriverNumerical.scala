package scalagrad.numerical

import scalagrad.api.Deriver
import scala.reflect.ClassTag

object DeriverNumerical:

    def approx[P](e: P)(using f: Fractional[P]): Deriver[P => P] {
        type dfInput = P
        type dfOutput = P
    }  = new Deriver[P => P] {
        import f.*

        override type dfInput = P
        override type dfOutput = P
        override def derive(f: P => P): dfInput => dfOutput = (x) =>
            (f(x + e) - f(x)) / e
    }

    def approx2[P](e: P)(using f: Fractional[P]): Deriver[(P, P) => P] {
        type dfInput = (P, P)
        type dfOutput = (P, P)
    } = new Deriver[(P, P) => P] {
        import f.*

        override type dfInput = (P, P)
        override type dfOutput = (P, P)
        override def derive(f: (P, P) => P): dfInput => dfOutput = (x1, x2) => (
            (f(x1 + e, x2) - f(x1, x2)) / e, 
            (f(x1, x2 + e) - f(x1, x2)) / e
        )
    }

    def approxVector[P](e: P)(using f: Fractional[P]): Deriver[Vector[P] => P] {
        type dfInput = Vector[P]
        type dfOutput = Vector[P]
    }  = new Deriver[Vector[P] => P] {
        import f.*

        override type dfInput = Vector[P]
        override type dfOutput = Vector[P]
        override def derive(f: Vector[P] => P): dfInput => dfOutput = (xs) => (
            for (keyX <- xs.indices)
                yield (f(xs.updated(keyX, xs(keyX) + e)) - f(xs)) / e
        ).toVector
    }

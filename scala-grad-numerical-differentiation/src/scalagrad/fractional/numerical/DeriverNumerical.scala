package scalagrad.fractional.numerical

import scalagrad.api.Deriver
import scala.reflect.ClassTag

/**
  * Implementation of Numerical Differentiation following the Difference quotient (also Newton quotient, Fermat's difference quotient) formula:
  * 
  * f'(x) = (f(x + e) - f(x)) / e
  */
object DeriverNumerical:

    trait DeriverP[P] extends Deriver[P => P] {
        override type dfInput = P
        override type dfOutput = P
    }

    trait DeriverPP[P] extends Deriver[(P, P) => P] {
        override type dfInput = (P, P)
        override type dfOutput = (P, P)
    }

    trait DeriverVP[P] extends Deriver[Vector[P] => P] {
        override type dfInput = Vector[P]
        override type dfOutput = Vector[P]
    }

    // Use central difference as default
    export CentralDifference.*

    object ForwardDifference:

        def approx[P](e: P)(using f: Fractional[P]): DeriverP[P] = new DeriverP[P] {
            import f.*
            override def derive(f: P => P): dfInput => dfOutput = (x) =>
                (f(x + e) - f(x)) / e
        }

        def approx2[P](e: P)(using f: Fractional[P]): DeriverPP[P] = new DeriverPP {
            import f.*

            override type dfInput = (P, P)
            override type dfOutput = (P, P)
            override def derive(f: (P, P) => P): dfInput => dfOutput = (x1, x2) => (
                (f(x1 + e, x2) - f(x1, x2)) / e, 
                (f(x1, x2 + e) - f(x1, x2)) / e
            )
        }

        def approxVector[P](e: P)(using f: Fractional[P]): DeriverVP[P] = new DeriverVP[P] {
            import f.*

            override type dfInput = Vector[P]
            override type dfOutput = Vector[P]
            override def derive(f: Vector[P] => P): dfInput => dfOutput = (xs) => (
                for (keyX <- xs.indices)
                    yield (f(xs.updated(keyX, xs(keyX) + e)) - f(xs)) / e
            ).toVector
        }

    object CentralDifference:

        def approx[P](e: P)(using f: Fractional[P]): DeriverP[P] = 
            import f.*
            val halfE = e / f.fromInt(2)
            new DeriverP[P] {
                override type dfInput = P
                override type dfOutput = P
                override def derive(f: P => P): dfInput => dfOutput = (x) =>
                    (f(x + halfE) - f(x - halfE)) / e
            }

        def approx2[P](e: P)(using f: Fractional[P]): DeriverPP[P] = 
            import f.*
            val halfE = e / f.fromInt(2)
            new DeriverPP {

                override type dfInput = (P, P)
                override type dfOutput = (P, P)
                override def derive(f: (P, P) => P): dfInput => dfOutput = (x1, x2) => (
                    (f(x1 + halfE, x2) - f(x1 - halfE, x2)) / e, 
                    (f(x1, x2 + halfE) - f(x1, x2 - halfE)) / e
                )
            }

        def approxVector[P](e: P)(using f: Fractional[P]): DeriverVP[P] = 
            import f.*
            val halfE = e / f.fromInt(2)
            new DeriverVP[P] {
                import f.*

                override type dfInput = Vector[P]
                override type dfOutput = Vector[P]
                override def derive(f: Vector[P] => P): dfInput => dfOutput = (xs) => (
                    for (keyX <- xs.indices)
                        yield (f(xs.updated(keyX, xs(keyX) + halfE)) - f(xs.updated(keyX, xs(keyX) - halfE))) / e
                ).toVector
            }

    object BackwardDifference:

        def approx[P](e: P)(using f: Fractional[P]): DeriverP[P] = new DeriverP[P] {
            import f.*

            override type dfInput = P
            override type dfOutput = P
            override def derive(f: P => P): dfInput => dfOutput = (x) =>
                (f(x) - f(x - e)) / e
        }

        def approx2[P](e: P)(using f: Fractional[P]): DeriverPP[P] = new DeriverPP {
            import f.*

            override type dfInput = (P, P)
            override type dfOutput = (P, P)
            override def derive(f: (P, P) => P): dfInput => dfOutput = (x1, x2) => (
                (f(x1, x2) - f(x1 - e, x2)) / e, 
                (f(x1, x2) - f(x1, x2 - e)) / e
            )
        }

        def approxVector[P](e: P)(using f: Fractional[P]): DeriverVP[P] = new DeriverVP[P] {
            import f.*

            override type dfInput = Vector[P]
            override type dfOutput = Vector[P]
            override def derive(f: Vector[P] => P): dfInput => dfOutput = (xs) => (
                for (keyX <- xs.indices)
                    yield (f(xs) - f(xs.updated(keyX, xs(keyX) - e))) / e
            ).toVector
        }

package scalagrad.forward.api

import scalagrad.api.Deriver
import scala.reflect.ClassTag

import breeze.linalg.DenseVector
import breeze.linalg.operators.OpAdd
import breeze.math.Field
import breeze.storage.Zero
import breeze.linalg.operators.OpSub


trait DeriverBreeze:

    // TODO how to keep Numerical "open", but still be useful definition?
    type DNum[P]

    given breezeDenseVectorToScalar[P: ClassTag](using Field[P]): Deriver[DenseVector[DNum[P]] => DNum[P]]

// TODO move Fractional out of here
object DeriverBreeze:

    given add[P](using f: Fractional[P]): OpAdd.Impl2[P, P, P] with
        def apply(a: P, b: P): P = 
            import f._
            a + b

    given sub[P](using f: Fractional[P]): OpSub.Impl2[P, P, P] with
        def apply(a: P, b: P): P = 
            import f._
            a - b

    given [P](using f: Fractional[P]): Zero[P] = Zero(f.zero)

    given [P](using f: Fractional[P]): Field[P] with

        import f._

        def zero = f.zero

        def one = f.one

        def nan = ???

        def ==(a: P, b: P) = f.equiv(a, b)

        def !=(a: P, b: P) = a != b

        def >(a: P, b: P) = a > b

        def >=(a: P, b: P) = a >= b

        def <(a: P, b: P) = a < b

        def <=(a: P, b: P) = a <= b

        def +(a: P, b: P) = a + b

        def -(a: P, b: P) = a - b

        def *(a: P, b: P) = a * b

        def /(a: P, b: P) = a / b

        def toDouble(a: P) = f.toDouble(a)

        def isNaN(a: P) = ???

        def pow(a: P, b: P): P = ???

        def %(a: P, b: P): P = ???

        def abs(a: P): P = f.abs(a)

        implicit val normImpl: breeze.linalg.norm.Impl[P, Double] =
            new breeze.linalg.norm.Impl[P, Double] {
                def apply(v: P): Double = f.abs(v).toDouble
            }
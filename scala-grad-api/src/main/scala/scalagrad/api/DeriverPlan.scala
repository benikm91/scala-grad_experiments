package scalagrad.api

import scalagrad.api.Deriver
import scala.reflect.ClassTag
import scala.math.Fractional
import scala.runtime.Tuples

object DeriverPlan:

    given single[P: Fractional, D, PD <: Dual[P, D, PD]](using frac: Fractional[P], vectorDeriver: DeriverFromTo[Vector[PD] => PD, Vector[P] => Vector[P]]): Deriver[PD => PD] with

        override type dfT = P => P

        override def derive(f: fT): dfT = 
            x => 
                val res = vectorDeriver.derive(x => 
                    assert(x.size == 1, f"${x.size} != 1")
                    f(x(0))
                )(Vector(x))
                assert(res.size == 1, f"${res.size} != 1")
                res(0)

    given tuple2[P: Fractional, D, PD <: Dual[P, D, PD]](using vectorDeriver: DeriverFromTo[Vector[PD] => PD, Vector[P] => Vector[P]]): Deriver[(PD, PD) => PD] with

        override type dfT = (P, P) => (P, P)

        override def derive(f: fT): dfT = 
            (x1, x2) =>
                val res = vectorDeriver.derive(x => 
                    assert(x.size == 2, f"${x.size} != 2")
                    f(x(0), x(1))
                )(Vector(x1, x2))
                assert(res.size == 2, f"${res.size} != 2")
                (res(0), res(1))

    given vector[P: Fractional, D, PD <: Dual[P, D, PD]](using vector2Vector: DeriverFromTo[Vector[PD] => Vector[PD], Vector[P] => Vector[Vector[P]]]): Deriver[Vector[PD] => PD] with

        override type dfT = Vector[P] => Vector[P]

        override def derive(f: fT): dfT = x =>
            val res = vector2Vector.derive(x => Vector(f(x)))(x)
            res.foreach(x => 
                assert(x.size == 1, f"${x.size} != 1")
            )
            res.map(_.head)
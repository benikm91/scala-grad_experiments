package scalagrad.api

import scalagrad.api.Deriver
import scala.reflect.ClassTag
import scala.math.Fractional
import scala.runtime.Tuples

trait DeriverPlan[P, D, PD <: Dual[P, D, PD]](using frac: Fractional[P]):

    type DeriverD[I, O] = Deriver[Vector[PD] => Vector[PD]] {
        type dfInput = I
        type dfOutput = O
    }

    given single: Deriver[PD => PD] with
        override type dfInput = P
        override type dfOutput = P
        override def derive(f: fT): dfT = 
            x => 
                val res = vector.derive(x => 
                    assert(x.size == 1, f"${x.size} != 1")
                    f(x(0))
                )(Vector(x))
                assert(res.size == 1, f"${res.size} != 1")
                res(0)

    given tuple2: Deriver[(PD, PD) => PD] with
        override type dfInput = (P, P)
        override type dfOutput = (P, P)
        override def derive(f: fT): dfT = 
            (x1, x2) =>
                val res = vector.derive(x => 
                    assert(x.size == 2, f"${x.size} != 2")
                    f(x(0), x(1))
                )(Vector(x1, x2))
                assert(res.size == 2, f"${res.size} != 2")
                (res(0), res(1))

    given vector: Deriver[Vector[PD] => PD] with
        override type dfInput = Vector[P]
        override type dfOutput = Vector[P]
        override def derive(f: fT): dfT = x =>
            val res = vector2Vector.derive(x => Vector(f(x)))(x)
            res.foreach(x => 
                assert(x.size == 1, f"${x.size} != 1")
            )
            res.map(_.head)

    given vector2Vector: DeriverD[Vector[P], Vector[Vector[P]]]
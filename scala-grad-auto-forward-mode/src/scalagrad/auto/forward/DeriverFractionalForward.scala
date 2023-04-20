package scalagrad.auto.forward

import scalagrad.api.Deriver
import scala.reflect.ClassTag
import scala.math.Fractional
import scala.runtime.Tuples

import scalagrad.auto.forward.dual.DualNumber

trait DeriverForward[fT2] extends Deriver[fT2]

trait DeriverForwardPlan[P: Fractional]:

    val one: P
    val zero: P

    given single: DeriverForward[DualNumber[P] => DualNumber[P]] with
        override type dfInput = P
        override type dfOutput = P
        override def derive(f: fT): dfT = 
            x => f(DualNumber[P](x, one)).dv

    given tuple2: DeriverForward[(DualNumber[P], DualNumber[P]) => DualNumber[P]] with
        override type dfInput = (P, P)
        override type dfOutput = (P, P)
        override def derive(f: fT): dfT = 
            (x1, x2) => (
                f(DualNumber[P](x1, one), DualNumber[P](x2, zero)).dv,
                f(DualNumber[P](x1, zero), DualNumber[P](x2, one)).dv
            )

    given vector: DeriverForward[Vector[DualNumber[P]] => DualNumber[P]] with
        override type dfInput = Vector[P]
        override type dfOutput = Vector[P]
        override def derive(f: fT): dfT = 
            (xs) => (
                for (i <- xs.indices) yield f((
                    for {
                        (x, j) <- xs.zipWithIndex
                        dxi = if i == j then one else zero
                    } yield DualNumber[P](x, dxi)
                )).dv
            ).toVector
    
object DeriverForwardPlan:

    object DeriverForwardPlanDouble extends DeriverForwardPlan[Double] {
        val one = 1.0
        val zero = 0.0
    }

    export DeriverForwardPlanDouble.given
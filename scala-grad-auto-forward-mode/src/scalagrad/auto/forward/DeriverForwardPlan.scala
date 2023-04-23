package scalagrad.auto.forward

import scalagrad.api.Deriver
import scalagrad.api.DeriverPlan
import scala.reflect.ClassTag
import scala.math.Fractional
import scala.runtime.Tuples

import scalagrad.auto.forward.dual.DualNumber

trait DeriverForwardPlan[P](using frac: Fractional[P]) extends DeriverPlan[P, P, DualNumber[P]]:

    given vector2Vector: Deriver[Vector[DualNumber[P]] => Vector[DualNumber[P]]] with
    
        override type dfInput = Vector[P]
        
        override type dfOutput = Vector[Vector[P]]
        
        override def derive(f: fT): dfT =
            xs =>(
                for (i <- xs.indices) yield f((
                    for {
                        (x, j) <- xs.zipWithIndex
                        dxi = if i == j then frac.one else frac.zero
                    } yield DualNumber[P](x, dxi)
                )).map(_.dv)
            ).toVector

    
object DeriverForwardPlan:

    object DeriverForwardPlanDouble extends DeriverForwardPlan[Double]

    export DeriverForwardPlanDouble.given
package scalagrad.spire.auto.forward

import scalagrad.api.Deriver
import scala.reflect.ClassTag
import scala.runtime.Tuples

import scalagrad.spire.api.DeriverSpireNumeric
import scalagrad.spire.auto.forward.dual.DualNumber
import spire.math.Numeric

import scalagrad.spire.api.DeriverSpireNumeric

trait DeriverSpireNumericForward[fT2] extends Deriver[fT2]

object DeriverSpireNumericForward extends DeriverSpireNumeric:

    type DNum[V] = DualNumber[V]

    given spireNumeric[P] (using num: Numeric[P]): DeriverSpireNumericForward[DNum[P] => DNum[P]] with
        override type dfInput = P
        override type dfOutput = P
        override def derive(f: fT): dfT = 
            x => f(DualNumber[P](x, num.one)).dv

    given spireNumeric2[P] (using num: Numeric[P]): DeriverSpireNumericForward[(DNum[P], DNum[P]) => DNum[P]] with
        override type dfInput = (P, P)
        override type dfOutput = (P, P)
        override def derive(f: fT): dfT = 
            (x1, x2) => (
                f(DualNumber[P](x1, num.one), DualNumber[P](x2, num.zero)).dv,
                f(DualNumber[P](x1, num.zero), DualNumber[P](x2, num.one)).dv
            )

    given spireNumericVector[P] (using num: Numeric[P]): DeriverSpireNumericForward[Vector[DNum[P]] => DNum[P]] with
        override type dfInput = Vector[P]
        override type dfOutput = Vector[P]
        override def derive(f: fT): dfT = 
            (xs) =>
                (
                    for (i <- xs.indices) yield f((
                        for {
                            (x, j) <- xs.zipWithIndex
                            dxi = if i == j then num.one else num.zero
                        } yield DualNumber[P](x, dxi)
                    )).dv
                ).toVector
    
    
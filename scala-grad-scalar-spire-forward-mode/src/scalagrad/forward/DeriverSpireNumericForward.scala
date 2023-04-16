package scalagrad.forward

import scalagrad.api.Deriver
import scala.reflect.ClassTag
import scala.runtime.Tuples

import scalagrad.api.DeriverSpireNumeric
import scalagrad.forward.dual.DualNumber
import spire.math.Numeric

trait DeriverSpireNumericForward[fT2] extends Deriver[fT2]

object DeriverSpireNumericForward extends DeriverSpireNumeric:

    type DNum[V] = DualNumber[V]

    given spireNumeric[P] (using frac: Numeric[P]): DeriverSpireNumericForward[DNum[P] => DNum[P]] with
        override type dfInput = P
        override type dfOutput = P
        override def derive(f: fT): dfT = 
            x => f(DualNumber[P](x, frac.one)).dv

    given spireNumeric2[P] (using frac: Numeric[P]): DeriverSpireNumericForward[(DNum[P], DNum[P]) => DNum[P]] with
        override type dfInput = (P, P)
        override type dfOutput = (P, P)
        override def derive(f: fT): dfT = 
            (x1, x2) => (
                f(DualNumber[P](x1, frac.one), DualNumber[P](x2, frac.zero)).dv,
                f(DualNumber[P](x1, frac.zero), DualNumber[P](x2, frac.one)).dv
            )

    given spireNumericVector[P] (using frac: Numeric[P]): DeriverSpireNumericForward[Vector[DNum[P]] => DNum[P]] with
        override type dfInput = Vector[P]
        override type dfOutput = Vector[P]
        override def derive(f: fT): dfT = 
            (xs) =>
                (
                    for (i <- xs.indices) yield f((
                        for {
                            (x, j) <- xs.zipWithIndex
                            dxi = if i == j then frac.one else frac.zero
                        } yield DualNumber[P](x, dxi)
                    )).dv
                ).toVector
    
    
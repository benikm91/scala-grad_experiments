package scalagrad.forward

import scalagrad.api.Deriver
import scalagrad.api.DeriverWithPrecision
import scala.reflect.ClassTag
import scala.math.Fractional
import scala.runtime.Tuples

import scalagrad.fractional.api.DeriverFractional

import scalagrad.forward.dual.DualNumber

trait DeriverFractionalForward[fT2] extends Deriver[fT2]

object DeriverFractionalForward extends DeriverFractional:

    type DNum[V] = DualNumber[V]

    given fractional[P] (using frac: Fractional[P]): DeriverFractionalForward[DNum[P] => DNum[P]] with
        override type dfInput = P
        override type dfOutput = P
        override def derive(f: fT): dfT = 
            x => f(DualNumber[P](x, frac.one)).dv

    given fractional2[P] (using frac: Fractional[P]): DeriverFractionalForward[(DNum[P], DNum[P]) => DNum[P]] with
        override type dfInput = (P, P)
        override type dfOutput = (P, P)
        override def derive(f: fT): dfT = 
            (x1, x2) => (
                f(DualNumber[P](x1, frac.one), DualNumber[P](x2, frac.zero)).dv,
                f(DualNumber[P](x1, frac.zero), DualNumber[P](x2, frac.one)).dv
            )

    given fractionalVector[P] (using frac: Fractional[P]): DeriverFractionalForward[Vector[DNum[P]] => DNum[P]] with
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
    
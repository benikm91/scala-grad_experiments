package scalagrad.forward

import scalagrad.api.Deriver
import scala.reflect.ClassTag
import scala.math.Fractional
import scala.runtime.Tuples

import scalagrad.fractional.api.DeriverFractional


trait DeriverForward[fT2] extends Deriver[fT2]

object DeriverForward extends DeriverFractional:

    type DNum[V] = DualNumber[V]

    given fractional[P] (using frac: Fractional[P]): DeriverForward[DNum[P] => DNum[P]] with
        override type dfInput = P
        override type dfOutput = P
        override def derive(f: fT): dfT = 
            x => f(DualNumber[P](x, frac.one)).dv

    given fractional2[P] (using frac: Fractional[P]): DeriverForward[(DNum[P], DNum[P]) => DNum[P]] with
        override type dfInput = (P, P)
        override type dfOutput = (P, P)
        override def derive(f: fT): dfT = 
            (x1, x2) => (
                f(DualNumber[P](x1, frac.one), DualNumber[P](x2, frac.zero)).dv,
                f(DualNumber[P](x1, frac.zero), DualNumber[P](x2, frac.one)).dv
            )

    given fractionalSeq[P] (using frac: Fractional[P]): DeriverForward[Seq[DNum[P]] => DNum[P]] with
        override type dfInput = Seq[P]
        override type dfOutput = Seq[P]
        override def derive(f: fT): dfT = 
            (xs) =>
                for (i <- xs.indices) yield f(
                    for {
                        (x, j) <- xs.zipWithIndex
                        dxi = if i == j then frac.one else frac.zero
                    } yield DualNumber[P](x, dxi)
                ).dv
    
    given fractionalArray[P : ClassTag] (using frac: Fractional[P]): DeriverForward[Array[DNum[P]] => DNum[P]] with
        override type dfInput = Array[P]
        override type dfOutput = Array[P]
        override def derive(f: fT): dfT = 
            (xs) =>
                (
                    for (i <- xs.indices) yield f((
                        for {
                            (x, j) <- xs.zipWithIndex
                            dxi = if i == j then frac.one else frac.zero
                        } yield DualNumber[P](x, dxi)
                    ).toArray).dv
                ).toArray
    
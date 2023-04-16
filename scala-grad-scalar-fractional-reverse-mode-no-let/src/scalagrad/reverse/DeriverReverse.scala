package scalagrad.reverse.nolet

import scalagrad.api.Deriver
import scalagrad.api.DeriverWithPrecision
import scala.reflect.ClassTag
import scala.math.Fractional
import scala.runtime.Tuples

import scalagrad.fractional.api.DeriverFractional
import scalagrad.reverse.nolet.dual.delta.Delta
import scalagrad.reverse.nolet.dual.DualDelta
import scalagrad.reverse.nolet.eval.Eval


trait DeriverFractionalReverse[fT2] extends Deriver[fT2]

object DeriverFractionalReverse extends DeriverFractional:

    type DNum[V] = DualDelta[V]

    given fractional[P] (using frac: Fractional[P]): DeriverFractionalReverse[DNum[P] => DNum[P]] with
        override type dfInput = P
        override type dfOutput = P
        override def derive(f: fT): dfT = 
            val keyX = 0
            def toDelta(x: P): Delta[P] = 
                val d1 = DualDelta[P](x, Delta.Val(keyX))
                f(d1).delta
            (x) => {
                val delta = toDelta(x)
                val dfs = Eval.eval(frac.one)(delta)(Vector(frac.zero))
                dfs(keyX)
            }

    given fractional2[P] (using frac: Fractional[P]): DeriverFractionalReverse[(DNum[P], DNum[P]) => DNum[P]] with
        override type dfInput = (P, P)
        override type dfOutput = (P, P)
        override def derive(f: fT): dfT = 
            val keyX1 = 0
            val keyX2 = 1
            def toDelta(x1: P, x2: P): Delta[P] = 
                val d1 = DualDelta[P](x1, Delta.Val(keyX1))
                val d2 = DualDelta[P](x2, Delta.Val(keyX2))
                f(d1, d2).delta
            (x1, x2) => {
                val delta = toDelta(x1, x2)
                val dfs = Eval.eval(frac.one)(delta)(Vector(frac.zero, frac.zero))
                (dfs(keyX1), dfs(keyX2))
            }

    given fractionalVector[P] (using frac: Fractional[P]): DeriverFractionalReverse[Vector[DNum[P]] => DNum[P]] with
        override type dfInput = Vector[P]
        override type dfOutput = Vector[P]
        override def derive(f: fT): dfT = 
            xs => {
                val keyXs = xs.indices
                def toDelta(xs: Vector[P]): Delta[P] = 
                    val ds = for ((x, keyX) <- xs.zip(keyXs))
                        yield DualDelta[P](x, Delta.Val(keyX))
                    f(ds).delta
                val delta = toDelta(xs)
                val dfs = Eval.eval(frac.one)(delta)(keyXs.map(x => frac.zero).toVector)
                (for (keyX <- keyXs) yield dfs(keyX)).toVector
            }
package scalagrad.fractional.auto.reverse

import scalagrad.api.Deriver
import scala.reflect.ClassTag
import scala.math.Fractional
import scala.runtime.Tuples

import scalagrad.fractional.api.DeriverFractional
import scalagrad.fractional.auto.reverse.dual.delta.{Delta, DeltaMonad}
import scalagrad.fractional.auto.reverse.dual.DualDelta
import scalagrad.fractional.auto.reverse.eval.Eval

import scalagrad.fractional.api.DeriverFractional

trait DeriverFractionalReverse[fT2] extends Deriver[fT2]

object DeriverFractionalReverse extends DeriverFractional:

    type DNum[V] = DualDelta[V]

    given fractional[P] (using frac: Fractional[P]): DeriverFractionalReverse[DNum[P] => DNum[P]] with
        override type dfInput = P
        override type dfOutput = P
        override def derive(f: fT): dfT = 
            val keyX = 0
            def toDelta(x: P): Delta[P] = 
                val d1 = DualDelta[P](x, DeltaMonad[P, Delta[P]](state => (state, Delta.Val(keyX))))
                val res = f(d1)
                Eval.runDelta(1, res.deltaM)
            (x) => {
                val delta = toDelta(x)
                // val dfs = Eval.eval(frac.one)(delta)(Map[Int, P]((keyX, frac.zero)))
                val dfs = Eval.evalNonRecursive(delta, Map[Int, P]((keyX, frac.zero)))
                dfs(keyX)
            }

    given fractional2[P] (using frac: Fractional[P]): DeriverFractionalReverse[(DNum[P], DNum[P]) => DNum[P]] with
        override type dfInput = (P, P)
        override type dfOutput = (P, P)
        override def derive(f: fT): dfT = 
            val keyX1 = 0
            val keyX2 = 1
            def toDelta(x1: P, x2: P): Delta[P] = 
                val d1 = DualDelta[P](x1, DeltaMonad[P, Delta[P]](state => (state, Delta.Val(keyX1))))
                val d2 = DualDelta[P](x2, DeltaMonad[P, Delta[P]](state => (state, Delta.Val(keyX2))))
                val res = f(d1, d2)
                Eval.runDelta(2, res.deltaM)
            (x1, x2) => {
                val delta = toDelta(x1, x2)
                // val dfs = Eval.eval(frac.one)(delta)(Map[Int, P]((keyX1, frac.zero), (keyX2, frac.zero)))
                val dfs = Eval.evalNonRecursive(delta, Map[Int, P]((keyX1, frac.zero), (keyX2, frac.zero)))
                (dfs(keyX1), dfs(keyX2))
            }

    given fractionalVector[P] (using frac: Fractional[P]): DeriverFractionalReverse[Vector[DNum[P]] => DNum[P]] with
        override type dfInput = Vector[P]
        override type dfOutput = Vector[P]
        override def derive(f: fT): dfT = 
            xs => {
                val keyXs = xs.indices
                def toDelta(xs: Vector[P]): Delta[P] = 
                    val duals = for ((x, keyX) <- xs.zip(keyXs))
                        yield DualDelta[P](x, DeltaMonad[P, Delta[P]](state => (state, Delta.Val(keyX))))
                    val res = f(duals)
                    Eval.runDelta(xs.size, res.deltaM)
                val delta = toDelta(xs)
                // val dfs = Eval.eval(frac.one)(delta)(keyXs.map((_, frac.zero)).toMap)
                val dfs = Eval.evalNonRecursive(delta, keyXs.map((_, frac.zero)).toMap)
                (for (keyX <- keyXs) yield dfs(keyX)).toVector
            }
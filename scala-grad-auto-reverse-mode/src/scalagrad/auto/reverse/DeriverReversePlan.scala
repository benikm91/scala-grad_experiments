package scalagrad.auto.reverse

import scalagrad.api.Deriver
import scalagrad.api.DeriverPlan
import scala.reflect.ClassTag
import scala.math.Fractional
import scala.runtime.Tuples

import scalagrad.auto.reverse.dual.delta.{Delta, DeltaMonad}
import scalagrad.auto.reverse.dual.DualDelta
import scalagrad.auto.reverse.eval.Eval

import scala.math.Fractional.Implicits.given

trait DeriverReversePlan[P: Fractional] extends DeriverPlan[P, DualDelta.D[P], DualDelta[P]]:

    val one: P
    val zero: P
    val fractional: Fractional[P]

    given vector2Vector: Deriver[Vector[DualDelta[P]] => Vector[DualDelta[P]]] with
        override type dfInput = Vector[P]
        override type dfOutput = Vector[Vector[P]]
        override def derive(f: fT): dfT = 
            xs => {
                val keyXs = xs.indices
                def toDelta(xs: Vector[P]): Vector[Delta[P]] = 
                    val duals = for ((x, keyX) <- xs.zip(keyXs))
                        yield DualDelta[P](x, DeltaMonad[P, Delta[P]](state => (state, Delta.Val(keyX))))
                    val resV = f(duals)
                    resV.map(res => Eval.runDelta(xs.size, res.deltaM))
                val deltas = toDelta(xs)
                // val dfs = Eval.eval(frac.one)(delta)(keyXs.map((_, frac.zero)).toMap)
                val dfsV = deltas.map(delta => 
                    Eval.evalNonRecursive(delta, keyXs.map((_, zero)).toMap)(using fractional)
                )
                dfsV.map(dfs =>
                    (for (keyX <- keyXs) yield dfs(keyX)).toVector
                ).transpose
            }

object DeriverReversePlan:

    object DeriverReversePlanDouble extends DeriverReversePlan[Double] {
        val fractional: Fractional[Double] = summon[Fractional[Double]]
        val one = 1.0
        val zero = 0.0
    }

    export DeriverReversePlanDouble.given
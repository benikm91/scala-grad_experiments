package scalagrad.reverse

import scalagrad.api.Deriver
import scala.reflect.ClassTag
import scala.math.Fractional
import scala.runtime.Tuples

import scalagrad.fractional.api.DeriverFractional
import scalagrad.reverse.delta.{Delta, DeltaMonad}
import scalagrad.reverse.dual.Dual
import scalagrad.reverse.eval.Eval


trait DeriverReverse[fT2] extends Deriver[fT2]

object DeriverReverse extends DeriverFractional:

    type DNum[V] = Dual[V]

    given fractional[P] (using frac: Fractional[P]): DeriverReverse[DNum[P] => DNum[P]] with
        override type dfInput = P
        override type dfOutput = P
        override def derive(f: fT): dfT = 
            ???

    given fractional2[P] (using frac: Fractional[P]): DeriverReverse[(DNum[P], DNum[P]) => DNum[P]] with
        override type dfInput = (P, P)
        override type dfOutput = (P, P)
        override def derive(f: fT): dfT = 
            val keyX1 = 0
            val keyX2 = 1
            def toDelta(x1: P, x2: P): Delta[P] = 
                val d1 = Dual[P](x1, DeltaMonad[P, Delta[P]](state => (state, Delta.Val(keyX1))))
                val d2 = Dual[P](x2, DeltaMonad[P, Delta[P]](state => (state, Delta.Val(keyX2))))
                val res = f(d1, d2)
                Eval.runDelta(2, res.deltaM)
            (x1, x2) => {
                val delta = toDelta(x1, x2)
                val dfs = Eval.eval(frac.one)(delta)(Map[Int, P]((keyX1, frac.zero), (keyX2, frac.zero)))
                (dfs(keyX1), dfs(keyX2))
            }

    given fractionalSeq[P] (using frac: Fractional[P]): DeriverReverse[Seq[DNum[P]] => DNum[P]] with
        override type dfInput = Seq[P]
        override type dfOutput = Seq[P]
        override def derive(f: fT): dfT = 
            ???
    
    given fractionalArray[P : ClassTag] (using frac: Fractional[P]): DeriverReverse[Array[DNum[P]] => DNum[P]] with
        override type dfInput = Array[P]
        override type dfOutput = Array[P]
        override def derive(f: fT): dfT = 
            ???
    
package scalagrad.reverse.nolet.eval

import scalagrad.reverse.nolet.dual.delta._
import scala.annotation.tailrec
import scala.collection.mutable

object Eval:

    case class CacheInfo(cacheHits: Int, cacheSize: Int)

    /**
     * This does the same as Monadic Translation from the paper, as we do not evaluate the same Delta multiple times.
     * 
     * The Var, Let concept from Monadic Translation is just implicit (by reference of the object in memory). This would get lost in translation/serialization.
     * For e.g. staging (XLA) Monadic Translation is therefore necessary!
     */
    def eval[P](output: P)(delta: Delta[P])(input: Vector[P])(using f: Fractional[P]): Vector[P] =
        import f.*
        var cacheHits = 0
        val cache = mutable.Map[Int, Vector[P]]()
        def evalInner(output: P)(delta: Delta[P])(input: Vector[P]): Vector[P] =
            val hashCode = System.identityHashCode(delta)
            if cache.contains(hashCode) then
                cacheHits += 1
                cache(hashCode)
            else
                val res = delta match
                    case Delta.Zero => input
                    case Delta.Add(d1, d2) => 
                        evalInner(output)(d2)(
                            evalInner(output)(d1)(input)
                        )
                    case Delta.Sub(d1, d2) => 
                        evalInner(-output)(d2)(
                            evalInner(output)(d1)(input)
                        )
                    case Delta.Mul(d, s) => 
                        evalInner(output * s.value)(d)(input)
                    case Delta.Div(d, s) => 
                        evalInner(output / s.value)(d)(input)
                    case Delta.Val(n) => 
                        input.updated(n, input(n) + output)
                delta match
                    case Delta.Val(id) => ()
                    case _ => () //cache.put(hashCode, res)
                res
        // println(CacheInfo(cacheHits, cache.size))
        evalInner(output)(delta)(input)

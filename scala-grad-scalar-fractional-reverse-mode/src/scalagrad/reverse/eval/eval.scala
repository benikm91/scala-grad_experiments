package scalagrad.reverse.eval

import scalagrad.reverse.delta._

object Eval:

    def runDelta[P](startId: DeltaId, deltaM: DeltaMonad[P, Delta[P]]): Delta[P] =
        def wrap(body: Delta[P], stateEntry: (DeltaId, Delta[P])) = 
            stateEntry match { 
                case (id, rhs) => Delta.Let(id, rhs, body) 
            }
        val (finalState, result) = deltaM.run(DeltaState.start[P](startId))
        finalState.bindings.foldLeft(result)(wrap)

    def eval[P](output: P)(delta: Delta[P])(um: Map[DeltaId, P])(using frac: Fractional[P]): Map[DeltaId, P] =
        import frac._
        delta match
            case Delta.Zero => um
            case Delta.Add(d1, d2) => 
                eval(output)(d2)(
                    eval(output)(d1)(um)
                )
            case Delta.Sub(d1, d2) => 
                eval(frac.negate(output))(d2)(eval(output)(d1)(um))
            case Delta.Val(id) => 
                um.updatedWith(id)(v => Some(v.getOrElse(frac.zero) + output))
            case Delta.Let(id, e1, e2) =>
                lazy val um2 = eval(output)(e2)(um)
                um2.get(id) match
                    case None => um2
                    case Some(value) => eval(value)(e1)(um2.removed(id))
            case Delta.Mul(d, c) => 
                eval(output * c.value)(d)(um)
            case Delta.Div(d, c) => 
                eval(output / c.value)(d)(um)

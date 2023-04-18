package scalagrad.fractional.auto.reverse.eval

import scalagrad.fractional.auto.reverse.dual.delta._
import scala.annotation.tailrec
import scala.collection.mutable

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

    
    def evalTailRec[P](output: P, delta: Delta[P], um: Map[DeltaId, P])(using frac: Fractional[P]): Map[DeltaId, P] =
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


    // Let(2,  Add(Val(0),Val(1)),   Let(3,     Add(Val(2),Val(0)),     Val(3)))
    /*
    1. Add(Val(0),Val(1))
    2. Val(0)
    3. Val(1)
    => deltas is empty, but laterDeltas is not
    4. Add(Val(2),Val(0))
    5. Val(2)
    6. Val(0)
    => deltas is empty, but laterDeltas is not
    7. Val(3)
    => deltas is empty, and laterDeltas is empty
    result = Map(0 -> 1.0, 1 -> 1.0, 2 -> 2.0, 3 -> 3.0)

    why is result(0) equal to 1? Because we have a bug in the eval function. The result is updated correctly but the output is not.
    The correction is:
    case Delta.Val(id) => 
        um.updatedWith(id)(v => Some(v.getOrElse(frac.zero) + output))
    */
    def evalNonRecursive[P](startDelta: Delta[P], startVals: Map[DeltaId, P])(using frac: Fractional[P]): Map[DeltaId, P] =
        import frac._
        val result = mutable.Map(startVals.toSeq: _*)
        val deltas = mutable.Stack((frac.one, startDelta))
        val laterDeltas = mutable.Stack[(DeltaId, Delta[P])]()
        while (deltas.nonEmpty) do {
            val (output, delta) = deltas.pop()
            delta match
                case Delta.Zero => ()
                case Delta.Add(d1, d2) => 
                    deltas.push((output, d1))
                    deltas.push((output, d2))
                case Delta.Sub(d1, d2) => 
                    deltas.push((output, d1))
                    deltas.push((frac.negate(output), d2))
                case Delta.Val(id) => 
                    result.updateWith(id)(v => Some(v.getOrElse(frac.zero) + output))
                case Delta.Let(id, e1, e2) =>
                    deltas.push((output, e2))
                    laterDeltas.push((id, e1))
                case Delta.Mul(d, c) => 
                    deltas.push((output * c.value, d))
                case Delta.Div(d, c) => 
                    deltas.push((output / c.value, d))
            if deltas.isEmpty then
                var nextDelta: Option[(P, Delta[P])] = None
                while(nextDelta.isEmpty && laterDeltas.nonEmpty) do
                    val (id, delta) = laterDeltas.pop()
                    if result.contains(id) then
                        nextDelta = Some((result(id), delta))
                nextDelta.foreach(deltas.push(_))
        }
        result.toMap

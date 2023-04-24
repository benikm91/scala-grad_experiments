package scalagrad.api

type DeriverFromTo[fT2, dfT2] = Deriver[fT2] { 
    type dfT = dfT2 
}

trait Deriver[fT2]:

    final type fT = fT2
    
    type dfT

    def derive(f: fT): (dfT)

    // Even more general with chain and fork operation?
    // single operator => Just chain
    // two operators chain and fork. 
    // Note modes materialize because of fork and distributive nature of fork and chain!


package scalagrad.api

trait Deriver[fT2]:

    type fT = fT2

    type dfInput
    type dfOutput

    type dfT = dfInput => dfOutput

    def derive(f: fT): (dfT)

    // Even more general with chain and fork operation?
    // single operator => Just chain
    // two operators chain and fork. 
    // Note modes materialize because of fork and distributive nature of fork and chain!


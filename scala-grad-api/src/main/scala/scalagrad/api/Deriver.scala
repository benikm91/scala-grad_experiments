package scalagrad.api

import scala.deriving.Mirror

// TODO maybe this is to genreal fT2 must be a Function, Function2, ... Function22
trait Deriver[fT2]:

    type fT = fT2

    type dfInput
    type dfOutput

    type dfT = dfInput => dfOutput

    def derive(f: fT): (dfT)


import scala.compiletime.summonAll

object Deriver:

    /*
    * Given a function f: C[P] => Double, where C is some case class and P is the precision/numerical type I want to provide the following:
    * val df = derive(f)
    * where
    * df: C[P] => C[P] and internally the wrapping with DualNumber is done.
    */
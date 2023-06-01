package scalagrad.api

object ScalaGrad:

    def derive[S, T](f: S => T)(using d: Deriver[S => T]): d.dfT = 
        d.derive(f)

    def derive[S, T](f: (S, S) => T)(using d: Deriver[(S, S) => T]): d.dfT = 
        d.derive(f)

    def derive[A, B, C, D, T](f: (A, B, C, D) => T)(using d: Deriver[(A, B, C, D) => T]): d.dfT = 
        d.derive(f)

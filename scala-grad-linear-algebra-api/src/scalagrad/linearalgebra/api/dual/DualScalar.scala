package scalagrad.linearalgebra.api.dual

trait DualScalar[P, D]:
    def v: P
    def dv: D
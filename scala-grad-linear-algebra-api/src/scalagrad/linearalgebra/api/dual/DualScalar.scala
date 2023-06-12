package scalagrad.linearalgebra.api.dual

import scalagrad.api.Dual

trait DualScalar[P, D, PD <: Dual[P, D, PD]](using f: Fractional[P]) extends Dual[P, D, PD]
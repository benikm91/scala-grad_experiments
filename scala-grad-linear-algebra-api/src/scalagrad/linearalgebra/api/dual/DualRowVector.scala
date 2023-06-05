package scalagrad.linearalgebra.api.dual

import breeze.linalg.{Transpose, DenseVector}

trait DualRowVector[P, D]:
    def v: Transpose[DenseVector[P]]
    def dv: D
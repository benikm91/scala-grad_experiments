package scalagrad.linearalgebra.api.dual

import breeze.linalg.DenseVector

trait DualColumnVector[P, +D]:
    def v: DenseVector[P]
    def dv: D
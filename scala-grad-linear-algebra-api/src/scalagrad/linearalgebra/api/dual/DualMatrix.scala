package scalagrad.linearalgebra.api.dual

import breeze.linalg.DenseMatrix

trait DualMatrix[P, +D]:
    def v: DenseMatrix[P]
    def dv: D
package scalagrad.linearalgebra.auto.reverse.delta

type Deltas[P] = DeltaScalar[P] | DeltaRowVector[P] | DeltaColumnVector[P] | DeltaMatrix[P]

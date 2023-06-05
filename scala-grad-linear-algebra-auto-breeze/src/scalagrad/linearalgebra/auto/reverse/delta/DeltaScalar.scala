package scalagrad.linearalgebra.auto.reverse.delta

import breeze.linalg.{Transpose, DenseVector}

enum DeltaScalar[P]:
    case Zero(x: P) // TODO how to get ride of x here?
    case Let(id: Int, rhs: Deltas[P], body: DeltaScalar[P])
    case Val(id: Int)
    case MultiplyVV(m1: DeltaRowVector[P], m2: DeltaColumnVector[P])
    case MultiplyRVDCV(v: Transpose[DenseVector[Double]], d: DeltaColumnVector[P])
    case ColumnDotProduct(v: Transpose[DenseVector[Double]], d: DeltaColumnVector[P])
    case RowDotProduct(d: DeltaRowVector[P], v: DenseVector[Double])
    case Add(s1: DeltaScalar[P], s2: DeltaScalar[P])
    case Sub(s1: DeltaScalar[P], s2: DeltaScalar[P])
    case Div(s1: DeltaScalar[P], f: P)
    case Scale(s1: DeltaScalar[P], f: P)
    case Sum(v: DeltaColumnVector[P], vectorLength: Int)

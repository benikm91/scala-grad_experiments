package scalagrad.tensor.breeze.auto

import breeze.linalg.DenseVector
import breeze.linalg.DenseMatrix

enum DeltaVector[+P]:
  case Zero
  case MulScalar(d1: DeltaVector[P], c2: DeltaVector.Const[P])
  case MulVector(d1: DeltaVector[P], d2: DeltaVector.ConstVector[P])
  case MulMatrix(d1: DeltaVector[P], d2: DeltaVector.ConstMatrix[P])
  case Add(d1: DeltaVector[P], d2: DeltaVector[P])
  case Sub(d1: DeltaVector[P], d2: DeltaVector[P])
  case Sum(d: DeltaVector[P])
  case Val(id: Int)

object DeltaVector:

  case class Const[+P](value: P)
  case class ConstVector[+P](value: DenseVector[P])
  case class ConstMatrix[+P](value: DenseMatrix[P])

  extension[P] (d1: DeltaVector[P])
    def *(c2: Const[P]): DeltaVector[P] = DeltaVector.MulScalar(d1, c2)
    def *(c2: ConstVector[P]): DeltaVector[P] = DeltaVector.MulVector(d1, c2)
    def *(c2: ConstMatrix[P]): DeltaVector[P] = DeltaVector.MulMatrix(d1, c2)
    def +(d2: DeltaVector[P]): DeltaVector[P] = DeltaVector.Add(d1, d2)
    def -(d2: DeltaVector[P]): DeltaVector[P] = DeltaVector.Sub(d1, d2)

  extension[P] (c1: Const[P])
    def *(d2: DeltaVector[P]): DeltaVector[P] = DeltaVector.MulScalar(d2, c1)

enum DeltaMatrix[+P]:
  opaque type DeltaId = Int
  case Zero

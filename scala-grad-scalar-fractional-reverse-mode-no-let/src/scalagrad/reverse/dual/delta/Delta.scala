package scalagrad.reverse.nolet.dual.delta

enum Delta[+P] {
  case Zero
  case Mul(d1: Delta[P], c2: Delta.Const[P])
  case Div(d1: Delta[P], c2: Delta.Const[P])
  case Add(d1: Delta[P], d2: Delta[P])
  case Sub(d1: Delta[P], d2: Delta[P])
  case Val(id: Int)
}

object Delta:

  case class Const[+P](value: P)

  extension[P] (d1: Delta[P])
    def /(c2: Const[P]): Delta[P] = Delta.Div(d1, c2)
    def *(c2: Const[P]): Delta[P] = Delta.Mul(d1, c2)
    def +(d2: Delta[P]): Delta[P] = Delta.Add(d1, d2)
    def -(d2: Delta[P]): Delta[P] = Delta.Sub(d1, d2)

  extension[P] (c1: Const[P])
    def /(d2: Delta[P]): Delta[P] = Delta.Div(d2, c1)
    def *(d2: Delta[P]): Delta[P] = Delta.Mul(d2, c1)
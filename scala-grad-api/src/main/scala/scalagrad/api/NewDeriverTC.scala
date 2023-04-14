package main.scala.scalagrad.api

/*
TODO:
Can I define operators afterhand (outside of library)
Should I use Spire for supported operations? e.g. Numerical
*/

trait Multiplication[T]: 
    def times(x1: T, x2: T): T

object Multiplication:

    given Multiplication[Double] with 
        def times(x1: Double, x2: Double): Double = x1 * x2

trait Exp[T]:
    def exp(x: T): T

object Exp:

    given Exp[Double] with 
        def exp(x: Double) = scala.math.exp(x)

type DualD[P, D2] = Dual[P] { 
    type D = D2
}

def createDual[DualImpl <: Dual[P], P, D](p: P, d: D): DualImpl = ???

sealed trait Dual[P]:
    type D
    val p: P
    val d: D

trait DeriveOp2[P, K[_], DualImpl <: Dual[P]]:
    def derive(d1: DualImpl, d2: DualImpl): d1.D

object Dual:

    given [P, DualImpl <: Dual[P]](using 
        mP: Multiplication[P], 
        mD: DeriveOp2[P, Multiplication, DualImpl]
    ): Multiplication[DualImpl] = new Multiplication[DualImpl] {
        def times(x1: DualImpl, x2: DualImpl) = createDual(
            mP.times(x1.p, x2.p), 
            mD.derive(x1, x2)
        )
    }

    extension[P, DualImpl <: Dual[P]](d1: DualImpl)
        def *(d2: DualImpl)(using m: Multiplication[DualImpl]) = 
            m.times(d1, d2)

case class Derivative(value: Double)

// Forward-mode
case class DualNumber(override val p: Double, override val d: Derivative) extends Dual[Double]:
    override type D = Derivative // TODO opaque type?

object DualNumber:
    given DeriveOp2[Double, Multiplication, DualNumber] with
        def derive(d1: DualNumber, d2: DualNumber) = Derivative(d1.p * d2.d.value + d2.p + d1.d.value)

// Reverse-mode
enum Delta[+P]:
    case Zero
    case Scale(p: P, d: Delta[P])
    case Add(d1: Delta[P], d2: Delta[P])

case class DualDelta(override val p: Double, override val d: Delta[Double]) extends Dual[Double]:

    override type D = Delta[Double]

object DualDelta:
    given DeriveOp2[Double, Multiplication, DualDelta] with
        def derive(d1: DualDelta, d2: DualDelta) = 
            Delta.Add(Delta.Scale(d1.p, d2.d), Delta.Scale(d2.p, d1.d))

def exampleF0[P](x: Dual[P])(using Multiplication[Dual[P]]): Dual[P] = x * x

def exampleF1(x: Dual[Double])(using Multiplication[Dual[Double]]): Dual[Double] = x * x

def exampleF2(x: DualNumber): DualNumber = 
    summon[Multiplication[DualNumber]]
    x * x

def exampleF2(x: DualDelta): DualDelta = x * x

def derive[P](f: DualNumber => DualNumber): Double => Double =
    x => f(DualNumber(x, Derivative(1.0))).d.value

type DualI[P] <: Dual[P]

def exampleG1[P, DualImpl <: Dual[P]](x: DualImpl)(using Multiplication[DualImpl]): DualImpl = x * x

def exampleG2[P](x: DualI[P])(using Multiplication[DualI[P]]): DualI[P] = x * x

// val dF0 = derive(exampleF0[Double])
// val dF1 = derive(exampleF1)
val dF2 = derive(exampleF2)
// val dF3 = derive(exampleF3) // TODO implement/show reverse mode
val dG1 = derive(exampleG1)
// val dG2 = derive(exampleG2[Double])

@main
def main() =
    println(dF2(5.0))
    println(dG1(5.0))
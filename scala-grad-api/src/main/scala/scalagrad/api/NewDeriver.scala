package main.scala.scalagrad.api

/*
TODO:
Can I define operators afterhand (outside of library)
Should I use Spire for supported operations? e.g. Numerical

trait Exp[T]:
    def exp(x: T): T

object Exp:

    given Exp[Double] with 
        def exp(x: Double) = scala.math.exp(x)

type DualD[P, D2] = Dual[P] { 
    type D = D2
}

def createDual[P, D](p: P, d: D): DualD[P, D] = ???

trait Dual[P]:
    type D
    val p: P
    val d: D

    def combine(primary: P => P, derivative: (P, D) => D) = createDual(primary(this.p), derivative(this.p, this.d))
    
    def combine2(primary: (P, P) => P, derivative: ((P, D), (P, D)) => D) = 
        (o: DualD[P, D]) => createDual(primary(this.p, o.p), derivative((this.p, this.d), (o.p, o.d)))

    def exp: DualD[P, D]
    def *(o: DualD[P, D]): DualD[P, D]
    def +(o: DualD[P, D]): DualD[P, D]


object Dual:

    given xxs[P, D](using expP: Exp[P], expD: Exp[D]): Exp[DualD[P, D]] = new Exp[DualD[P, D]] {
        def exp(x: DualD[P, D]) = createDual(expP.exp(x.p), expD.exp(x.d))
    }


trait DualDouble extends Dual[Double]:
    def dExp(p: Double, d: D): D
    def exp = combine(scala.math.exp, dExp)

    def dTimes(d1: (Double, D), d2: (Double, D)): D 
    def *(o: DualD[Double, D]) = combine2(_ * _, dTimes)(o)
    def dAdd(d1: (Double, D), d2: (Double, D)): D 
    def +(o: DualD[Double, D]) = combine2(_ * _, dAdd)(o)


// Forward-mode
case class DualNumber(override val p: Double, override val d: Double) extends DualDouble:
    override type D = Double
    override def dExp(d: Double, p: Double): DualNumber#D = d * scala.math.exp(p)
    override def dTimes(dual1: (Double, Double), dual2: (Double, Double)): DualNumber#D = 
        val (p1, d1) = dual1
        val (p2, d2) = dual2
        p1 * d2 + p2 * d1
    
    override def dAdd(dual1: (Double, Double), dual2: (Double, Double)): DualNumber#D = 
        val (_, d1) = dual1
        val (_, d2) = dual2
        d1 + d2
 

// Reverse-mode
enum Delta[+P]:
    case Zero
    case Scale(p: P, d: Delta[P])
    case Add(d1: Delta[P], d2: Delta[P])

case class DualDelta(override val p: Double, override val d: Delta[Double]) extends DualDouble:

    override type D = Delta[Double]
    override def dExp(p: Double, d: Delta[Double]): Delta[Double] = Delta.Scale(scala.math.exp(p), d)

    override def dTimes(dual1: (Double, Delta[Double]), dual2: (Double, Delta[Double])): DualDelta#D = 
        val (p1, d1) = dual1
        val (p2, d2) = dual2
        Delta.Add(Delta.Scale(p1, d2), Delta.Scale(p2, d1))

    override def dAdd(dual1: (Double, Delta[Double]), dual2: (Double, Delta[Double])): DualDelta#D = 
        val (_, d1) = dual1
        val (_, d2) = dual2
        Delta.Add(d1, d2)


def exampleF(x: Dual[Double]): Dual[Double] = x.exp

object Extern:

    extension(dual: Dual[Double])
        def doIt = println("DOIT")
        def log = createDual(
            scala.math.log(dual.p),
            dual.d // Can I do this for Dual Type?
        )

    
    extension(dual: DualNumber)
        def log = createDual(
            scala.math.log(dual.p),
            1 / dual.d // ???
        )
*/
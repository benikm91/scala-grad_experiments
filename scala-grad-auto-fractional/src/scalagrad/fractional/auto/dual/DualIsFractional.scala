package scalagrad.fractional.auto.dual

import scala.math.Fractional
import scalagrad.auto.forward.dual.DualNumber
import scalagrad.api.Dual
import scalagrad.api.CreateDual

object DualIsFractional:

  given frac[T, D, PD <: Dual[T, D, PD]](using f: Fractional[T], cd: CreateDual[T, D, PD]): Fractional[PD] with

    import f.*
    import cd.*
    
    override def div(x: PD, y: PD): PD = 
      x / y

    override def fromInt(x: Int): PD = createEmpty(f.fromInt(x))

    override def compare(x: PD, y: PD): Int = f.compare(x.v, y.v)

    override def times(x: PD, y: PD): PD = x * y

    override def toFloat(x: PD): Float = f.toFloat(x.v)

    override def toInt(x: PD): Int = f.toInt(x.v)

    override def minus(x: PD, y: PD): PD = x - y

    override def toLong(x: PD): Long = f.toLong(x.v)

    override def plus(x: PD, y: PD): PD = x + y

    override def toDouble(x: PD): Double = f.toDouble(x.v)

    override def parseString(str: String): Option[PD] = None

    override def negate(x: PD): PD = 
      create(
        f.negate(x.v), 
        // x.dv * f.negate(f.one)
        x.scale(x.dv, f.negate(f.one))
      )

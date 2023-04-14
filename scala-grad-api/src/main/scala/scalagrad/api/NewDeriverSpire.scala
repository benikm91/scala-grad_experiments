package main.scala.scalagrad.api.spire

import spire.algebra._
import spire.implicits._
import spire.math.Numeric
import spire.math.Rational
import spire.math.ConvertableFrom
import spire.math.ConvertableTo
import spire.math.Algebraic
import spire.math.Real
import algebra.ring.AdditiveCommutativeMonoid

def example1[T: Numeric](x: T): T = x * x

case class DualNumber[T](v: T, dv: T)

enum Delta[+T]:
    case Zero
    case Scale(s: T, d: Delta[T])

case class DualDelta[T](v: T, delta: Delta[T])

trait DNumeric[T] extends Numeric

/*
 * ”Tutorial" schreiben + selbsterklärende Dokumentation, Quickstart
 * => Liste für nächstes mal
 * => Use cases ohne auto diff implementieren und dann AutoDiff einbauen
 * Spezifizieren von generellen use cases unterstützen?
 * Matrix determiante?
 * Weitere?
*/

object DualNumber:
    
    given [T](using num: Numeric[T]): Numeric[DualNumber[T]] = new Numeric[DualNumber[T]] {
        
        def lift(v: T): DualNumber[T] = DualNumber(v, num.zero)

        override def toString(a: DualNumber[T]): String = a.toString

        override def toFloat(a: DualNumber[T]): Float = num.toFloat(a.v)

        override def fromDouble(n: Double): DualNumber[T] = lift(num.fromDouble(n))

        override def fpow(a: DualNumber[T], b: DualNumber[T]): DualNumber[T] = ???

        override def div(x: DualNumber[T], y: DualNumber[T]): DualNumber[T] = ???

        override def plus(x: DualNumber[T], y: DualNumber[T]): DualNumber[T] = ???

        override def toByte(a: DualNumber[T]): Byte = ???

        override def toShort(a: DualNumber[T]): Short = ???

        override def one: DualNumber[T] = ???

        override def toReal(a: DualNumber[T]): Real = ???

        override def floor(a: DualNumber[T]): DualNumber[T] = ???

        override def fromReal(n: Real): DualNumber[T] = ???

        override def isWhole(a: DualNumber[T]): Boolean = ???

        override def fromFloat(n: Float): DualNumber[T] = ???

        override def fromRational(n: Rational): DualNumber[T] = ???

        override def toType[B: ConvertableTo](a: DualNumber[T]): B = ???

        override def ceil(a: DualNumber[T]): DualNumber[T] = ???

        override def toInt(a: DualNumber[T]): Int = ???

        override def fromShort(n: Short): DualNumber[T] = ???

        override def order: Order[DualNumber[T]] = ???

        override def nroot(a: DualNumber[T], n: Int): DualNumber[T] = ???

        override def toBigDecimal(a: DualNumber[T]): BigDecimal = ???

        override def compare(x: DualNumber[T], y: DualNumber[T]): Int = ???

        override def round(a: DualNumber[T]): DualNumber[T] = ???

        override def toRational(a: DualNumber[T]): Rational = ???

        override def toDouble(a: DualNumber[T]): Double = ???

        override def fromLong(n: Long): DualNumber[T] = ???

        override def fromType[B: ConvertableFrom](b: B): DualNumber[T] = ???

        override def times(x: DualNumber[T], y: DualNumber[T]): DualNumber[T] = ???

        override def toNumber(a: DualNumber[T]): spire.math.Number = ???

        override def toBigInt(a: DualNumber[T]): BigInt = ???

        override def toAlgebraic(a: DualNumber[T]): Algebraic = ???

        override def toLong(a: DualNumber[T]): Long = ???

        override def fromAlgebraic(n: Algebraic): DualNumber[T] = ???

        override def additiveCommutativeMonoid: AdditiveCommutativeMonoid[DualNumber[T]] = ???

        override def fromByte(n: Byte): DualNumber[T] = ???

        override def fromBigDecimal(n: BigDecimal): DualNumber[T] = ???

        override def negate(x: DualNumber[T]): DualNumber[T] = ???

        override def signum(a: DualNumber[T]): Int = ???

        override def zero: DualNumber[T] = ???

        override def fromInt(n: Int): DualNumber[T] = DualNumber(num.fromInt(n), num.zero)

        override def abs(a: DualNumber[T]): DualNumber[T] = ???
    }
    

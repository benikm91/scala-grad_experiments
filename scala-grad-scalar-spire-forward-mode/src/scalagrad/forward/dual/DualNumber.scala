package scalagrad.forward.dual

import spire.math.Numeric
import algebra.ring.MultiplicativeGroup
import algebra.ring.AdditiveCommutativeMonoid
import spire.math.Real
import spire.math.Algebraic
import spire.math.ConvertableTo
import cats.kernel.Order
import spire.math.ConvertableFrom
import spire.math.Rational
import spire.implicits._

case class DualNumber[T](value: T, derivative: T):
    inline def v: T = value
    inline def dv: T = derivative

object DualNumber:

    type T = Double

    given [T](using num: Numeric[T]): Numeric[DualNumber[T]] with

        def chain(f: T => T, df: T => T)(dn: DualNumber[T]) = 
            DualNumber(f(dn.v), df(dn.v) * dn.dv)

        def applyBoth(f: T => T)(dn: DualNumber[T]) =
            DualNumber(f(dn.v), f(dn.dv))

        def lift(v: T) = DualNumber(v, num.zero)

        override def fromShort(n: Short): DualNumber[T] = lift(num.fromShort(n))

        override def toFloat(a: DualNumber[T]): Float = num.toFloat(a.value)

        override def one: DualNumber[T] = lift(num.one)

        override def fromReal(n: Real): DualNumber[T] = lift(num.fromReal(n))

        override def floor(a: DualNumber[T]): DualNumber[T] = 
            def dFloor(v: T): T = 
                // TODO what to do here?
                // f(x) = x + 0.5 approximates the step function of ceil best, which derivative would be 1
                return num.one
            chain(num.ceil, dFloor)(a)

        override def plus(x: DualNumber[T], y: DualNumber[T]): DualNumber[T] = 
            DualNumber(x.v + y.v, x.dv + y.dv)

        override def toDouble(a: DualNumber[T]): Double = num.toDouble(a.v)

        override def negate(x: DualNumber[T]): DualNumber[T] = applyBoth(num.negate)(x)

        override def fromType[B: ConvertableFrom](b: B): DualNumber[T] = lift(summon[ConvertableFrom[B]].toType[T](b))

        override def fromAlgebraic(n: Algebraic): DualNumber[T] = ???

        override def toReal(a: DualNumber[T]): Real = num.toReal(a.value)

        override def signum(a: DualNumber[T]): Int = 
            num.signum(a.v)

        override def toType[B: ConvertableTo](a: DualNumber[T]): B = 
            summon[ConvertableTo[B]].fromType[T](a.v)

        override def toNumber(a: DualNumber[T]): spire.math.Number = num.toNumber(a.v)

        override def ceil(a: DualNumber[T]): DualNumber[T] =
            def dCeil(v: T): T = 
                // TODO what to do here?
                // f(x) = x - 0.5 approximates the step function of ceil best, which derivative would be 1
                return num.one
            chain(num.ceil, dCeil)(a)

        override def toBigInt(a: DualNumber[T]): BigInt = num.toBigInt(a.v)

        override def abs(a: DualNumber[T]): DualNumber[T] =
            def dAbs(v: T): T = 
                if (num.signum(v) == 0) then
                    ???  // TODO what to do here?
                else
                    num.fromInt(num.signum(v))
            chain(num.abs, dAbs)(a)

        override def round(a: DualNumber[T]): DualNumber[T] =
            def dRound(v: T): T = 
                // TODO what to do here?
                // f(x) = x approximates the step function of ceil best, which derivative would be 1
                return num.one
            chain(num.round, dRound)(a)

        override def fromBigDecimal(n: BigDecimal): DualNumber[T] = lift(num.fromBigDecimal(n))

        // fpow = f(ractional) pow(er)
        override def fpow(a: DualNumber[T], b: DualNumber[T]): DualNumber[T] = 
            def log(x: T): T = ???  // TODO Trigometric function not part of Numeric :( => add trigometric type class
            // Source: https://www.wolframalpha.com/input?i=f%28x%29%5Eg%28x%29+derivative
            val dfpow = num.fpow(a.v, b.v - 1) * (b.v * a.dv + a.v * log(a.v) * b.dv)
            DualNumber(num.fpow(a.v, b.v), dfpow)


        override def fromDouble(n: Double): DualNumber[T] = lift(num.fromDouble(n))

        override def fromLong(n: Long): DualNumber[T] = lift(num.fromLong(n))

        override def toBigDecimal(a: DualNumber[T]): BigDecimal = num.toBigDecimal(a.v)

        override def times(x: DualNumber[T], y: DualNumber[T]): DualNumber[T] = 
            def productRule(u: T, du: T, v: T, dv: T): T = 
                du * v + u * dv
            DualNumber[T](x.v * y.v, productRule(x.v, x.dv, y.v, y.dv))

        override def fromRational(n: Rational): DualNumber[T] = 
            lift(num.fromRational(n))

        override def toString(a: DualNumber[T]): String = a.toString

        override def toShort(a: DualNumber[T]): Short = num.toShort(a.v)

        override def toInt(a: DualNumber[T]): Int = num.toInt(a.v)

        override def zero: DualNumber[T] = lift(num.zero)

        override def toAlgebraic(a: DualNumber[T]): Algebraic = ???

        override def toRational(a: DualNumber[T]): Rational = num.toRational(a.v)

        override def compare(x: DualNumber[T], y: DualNumber[T]): Int = 
            num.compare(x.v, y.v)

        override def div(x: DualNumber[T], y: DualNumber[T]): DualNumber[T] = 
            def quotientRule(u: T, du: T, v: T, dv: T): T =
                (du * v - u * dv) / (v * v)
            DualNumber[T](x.v / y.v, quotientRule(x.v, x.dv, y.v, y.dv))

        override def order: Order[DualNumber[T]] = new Order {
          override def compare(x: DualNumber[T], y: DualNumber[T]): Int = num.order.compare(x.v, y.v)
        }

        override def toByte(a: DualNumber[T]): Byte = num.toByte(a.v)

        override def nroot(a: DualNumber[T], n: Int): DualNumber[T] = 
            chain(x => num.nroot(x, n), x => num.fpow(x, num.one / num.fromInt(n - 1)) / n)(a)

        override def fromByte(n: Byte): DualNumber[T] = lift(num.fromByte(n))

        override def additiveCommutativeMonoid: AdditiveCommutativeMonoid[DualNumber[T]] = ???

        override def toLong(a: DualNumber[T]): Long = num.toLong(a.v)

        override def fromFloat(n: Float): DualNumber[T] = lift(num.fromFloat(n))

        override def isWhole(a: DualNumber[T]): Boolean = num.isWhole(a.v)

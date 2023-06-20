package scalagrad.spire.auto.dual

import scalagrad.auto.forward.dual.DualNumber

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
import spire.algebra.{AdditiveAbGroup, IsReal, MultiplicativeAbGroup, NRoot, Ring}
import scala.math.Equiv.ExtraImplicits
import algebra.ring.AdditiveMonoid
import algebra.ring.MultiplicativeSemigroup
import algebra.ring.MultiplicativeMonoid
import algebra.ring.Field
import spire.math.Number
import algebra.ring.Signed
import algebra.ring.Signed.Zero
import algebra.ring.Signed.Positive
import algebra.ring.Signed.Negative
import algebra.ring.AdditiveGroup
import spire.algebra.Trig
import scalagrad.api.Dual
import scalagrad.api.CreateDual

object DualIsNumeric:

    given [T, D, PD <: Dual[T, D, PD]](using num: Numeric[PD]): Field[PD] with
        
        override def one: PD = num.one

        override def times(x: PD, y: PD): PD = x * y

        override def plus(x: PD, y: PD): PD = x + y

        override def zero: PD = num.zero

        override def negate(x: PD): PD = -x

        override def div(x: PD, y: PD): PD = x / y


    given [T, D, PD <: Dual[T, D, PD]](using num: Numeric[T], cd: CreateDual[T, D, PD], trig: Trig[T]): Numeric[PD] with

        def chain(f: T => T, df: T => T)(dn: PD) =
            cd.create(
                f(dn.v), 
                dn.scale(dn.dv, df(dn.v))
                // dn.dv |*| df(dn.v)
            )

        def lift(v: T) = cd.createEmpty(v)

        override def fromShort(n: Short): PD = lift(num.fromShort(n))

        override def toFloat(a: PD): Float = num.toFloat(a.v)

        override def one: PD = lift(num.one)

        override def fromReal(n: Real): PD = lift(num.fromReal(n))

        override def floor(a: PD): PD = 
            def dFloor(v: T): T = 
                return num.zero
            chain(num.ceil, dFloor)(a)

        override def plus(x: PD, y: PD): PD = x + y

        override def toDouble(a: PD): Double = num.toDouble(a.v)

        override def negate(x: PD): PD = 
            cd.create(
                num.negate(x.v), 
                // x.dv * f.negate(f.one)
                x.scale(x.dv, num.negate(num.one))
            )

        override def fromType[B: ConvertableFrom](b: B): PD = lift(summon[ConvertableFrom[B]].toType[T](b))

        override def fromAlgebraic(n: Algebraic): PD = ???

        override def toReal(a: PD): Real = num.toReal(a.v)

        override def signum(a: PD): Int = 
            num.signum(a.v)

        override def toType[B: ConvertableTo](a: PD): B = 
            summon[ConvertableTo[B]].fromType[T](a.v)

        override def toNumber(a: PD): spire.math.Number = num.toNumber(a.v)

        override def ceil(a: PD): PD =
            def dCeil(v: T): T = 
                return num.zero
            chain(num.ceil, dCeil)(a)

        override def toBigInt(a: PD): BigInt = num.toBigInt(a.v)

        override def abs(a: PD): PD =
            def dAbs(v: T): T = 
                num.fromInt(num.signum(v))
            chain(num.abs, dAbs)(a)

        override def round(a: PD): PD =
            def dRound(v: T): T = 
                return num.zero
            chain(num.round, dRound)(a)

        override def fromInt(n: Int): PD = lift(num.fromInt(n))

        override def fromBigInt(n: BigInt): PD = 
            lift(num.fromBigInt(n))

        override def fromBigDecimal(n: BigDecimal): PD = lift(num.fromBigDecimal(n))

        // fpow = f(ractional) pow(er)
        override def fpow(a: PD, b: PD): PD = 
            def log(x: T): T = trig.log(x)
            // Source: https://www.wolframalpha.com/input?i=f%28x%29%5Eg%28x%29+derivative
            // val dfpow = num.fpow(a.v, b.v - 1) * (b.v * a.dv + a.v * log(a.v) * b.dv)
            val dfpow = a.scale((a.addD(a.scale(a.dv, b.v), a.scale(b.dv, a.v * log(a.v)))), num.fpow(a.v, b.v - 1))
            cd.create(num.fpow(a.v, b.v), dfpow)

        override def fromDouble(n: Double): PD = lift(num.fromDouble(n))

        override def fromLong(n: Long): PD = lift(num.fromLong(n))

        override def toBigDecimal(a: PD): BigDecimal = num.toBigDecimal(a.v)

        override def times(x: PD, y: PD): PD = x * y

        override def fromRational(n: Rational): PD = 
            lift(num.fromRational(n))

        override def toString(a: PD): String = a.toString

        override def toShort(a: PD): Short = num.toShort(a.v)

        override def toInt(a: PD): Int = num.toInt(a.v)

        override def zero: PD = lift(num.zero)

        override def toAlgebraic(a: PD): Algebraic = ???

        override def toRational(a: PD): Rational = num.toRational(a.v)

        override def compare(x: PD, y: PD): Int = 
            num.compare(x.v, y.v)

        override def div(x: PD, y: PD): PD = x / y

        override def order: Order[PD] = new Order {
          override def compare(x: PD, y: PD): Int = num.order.compare(x.v, y.v)
        }

        override def toByte(a: PD): Byte = num.toByte(a.v)

        override def nroot(a: PD, n: Int): PD = 
            def dnroot(x: T) = 
                val denominator = n * num.fpow((num.nroot(x, n)), num.fromInt(n - 1))
                num.one / denominator
            chain(x => num.nroot(x, n), dnroot)(a)

        override def fromByte(n: Byte): PD = lift(num.fromByte(n))

        override def additiveCommutativeMonoid: AdditiveCommutativeMonoid[PD] = ???

        override def toLong(a: PD): Long = num.toLong(a.v)

        override def fromFloat(n: Float): PD = lift(num.fromFloat(n))

        override def isWhole(a: PD): Boolean = num.isWhole(a.v)

    given [A, D, PD <: Dual[A, D, PD]](using trig: Trig[A], cd: CreateDual[A, D, PD], num: Numeric[A], nRoot: NRoot[A]): Trig[PD] with

        def chain(f: A => A, df: A => A)(dn: PD) =
            cd.create(
                f(dn.v), 
                dn.scale(dn.dv, df(dn.v))
                // dn.dv |*| df(dn.v)
            )

        def lift(a: A): PD = cd.createEmpty(a)

        override def e: PD = lift(trig.e)
        override def pi: PD = lift(trig.pi)
        def dExp(a: A): A = trig.exp(a)
        override def exp(a: PD): PD = chain(trig.exp, dExp)(a)
        override def expm1(a: PD): PD = chain(trig.expm1, dExp)(a)
        override def log(a: PD): PD = 
            def dLog(a: A): A = num.one / a
            chain(trig.log, dLog)(a)
        override def log1p(a: PD): PD = 
            def dLog1p(a: A): A = num.one / (1 + a)
            chain(trig.log1p, dLog1p)(a)
        override def sin(a: PD): PD = 
            def dSin(v: A): A = trig.cos(v)
            chain(trig.sin, dSin)(a)
        override def cos(a: PD): PD = 
            def dCos(v: A): A = -trig.sin(v)
            chain(trig.cos, dCos)(a)
        override def tan(a: PD): PD = 
            def dTan(v: A): A = num.one / (trig.cos(v) * trig.cos(v))
            chain(trig.tan, dTan)(a)
        override def asin(a: PD): PD = 
            def dAsin(v: A): A = num.one / nRoot.sqrt(1 - v * v)
            chain(trig.asin, dAsin)(a)
        override def acos(a: PD): PD =
            def dAcos(v: A): A = - (num.one / nRoot.sqrt(1 - v * v))
            chain(trig.acos, dAcos)(a)
        override def atan(a: PD): PD = 
            def dAtan(v: A): A = num.one / (v * v + num.one)
            chain(trig.atan, dAtan)(a)
        override def atan2(y: PD, x: PD): PD = ???
        override def sinh(x: PD): PD = 
            def dSinh(v: A): A = trig.cosh(v)
            chain(trig.sinh, dSinh)(x)
        override def cosh(x: PD): PD =
            def dCosh(v: A): A = trig.sinh(v)
            chain(trig.cosh, dCosh)(x)
        override def tanh(x: PD): PD =
            def dTanh(v: A): A = num.one / (trig.cosh(v) * trig.cosh(v))
            chain(trig.tanh, dTanh)(x)
        override def toRadians(a: PD): PD = 
            def dToRadians(v: A): A = trig.pi / num.fromInt(180)
            chain(trig.toRadians, dToRadians)(a)
        override def toDegrees(a: PD): PD =
            def dToDegrees(v: A): A = num.fromInt(180) / trig.pi
            chain(trig.toDegrees, dToDegrees)(a)
 

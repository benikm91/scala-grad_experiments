package scalagrad.spire.auto.forward.dual

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

object DualNumberIsNumeric:

    def chain[T: MultiplicativeMonoid](f: T => T, df: T => T)(dn: DualNumber[T]) = 
        DualNumber(f(dn.v), df(dn.v) * dn.dv)

    def applyBoth[T](f: T => T)(dn: DualNumber[T]) =
        DualNumber(f(dn.v), f(dn.dv))
       
    given [T](using num: Numeric[DualNumber[T]]): Field[DualNumber[T]] with
        
        override def one: DualNumber[T] = num.one

        override def times(x: DualNumber[T], y: DualNumber[T]): DualNumber[T] = x * y

        override def plus(x: DualNumber[T], y: DualNumber[T]): DualNumber[T] = x + y

        override def zero: DualNumber[T] = num.zero

        override def negate(x: DualNumber[T]): DualNumber[T] = -x

        override def div(x: DualNumber[T], y: DualNumber[T]): DualNumber[T] = x / y


    given [T](using num: Numeric[T], trig: Trig[T]): Numeric[DualNumber[T]] with

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
            def log(x: T): T = trig.log(x)
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
            def dnroot(x: T) = 
                val denominator = n * num.fpow((num.nroot(x, n)), num.fromInt(n - 1))
                num.one / denominator
            chain(x => num.nroot(x, n), dnroot)(a)

        override def fromByte(n: Byte): DualNumber[T] = lift(num.fromByte(n))

        override def additiveCommutativeMonoid: AdditiveCommutativeMonoid[DualNumber[T]] = ???

        override def toLong(a: DualNumber[T]): Long = num.toLong(a.v)

        override def fromFloat(n: Float): DualNumber[T] = lift(num.fromFloat(n))

        override def isWhole(a: DualNumber[T]): Boolean = num.isWhole(a.v)

    given [A: Field](using trig: Trig[A], ag: AdditiveGroup[A], nRoot: NRoot[A]): Trig[DualNumber[A]] with

        def lift(a: A): DualNumber[A] = DualNumber(a, ag.zero)

        override def e: DualNumber[A] = lift(trig.e)
        override def pi: DualNumber[A] = lift(trig.pi)
        override def exp(a: DualNumber[A]): DualNumber[A] = 
            def dExp(v: A): A = trig.exp(v)
            chain(trig.exp, dExp)(a)
        override def expm1(a: DualNumber[A]): DualNumber[A] = ???
        override def log(a: DualNumber[A]): DualNumber[A] = ???
        override def log1p(a: DualNumber[A]): DualNumber[A] = ???
        override def sin(a: DualNumber[A]): DualNumber[A] = 
            def dSin(v: A): A = trig.cos(v)
            chain(trig.sin, dSin)(a)
        override def cos(a: DualNumber[A]): DualNumber[A] = 
            def dCos(v: A): A = -trig.sin(v)
            chain(trig.cos, dCos)(a)
        override def tan(a: DualNumber[A]): DualNumber[A] = 
            def dTan(v: A): A = 1 / (trig.cos(v) * trig.cos(v))
            chain(trig.tan, dTan)(a)
        override def asin(a: DualNumber[A]): DualNumber[A] = 
            def dAsin(v: A): A = 1 / nRoot.sqrt(1 - v * v)
            chain(trig.asin, dAsin)(a)
        override def acos(a: DualNumber[A]): DualNumber[A] =
            def dAcos(v: A): A = - (1 / nRoot.sqrt(1 - v * v))
            chain(trig.acos, dAcos)(a)
        override def atan(a: DualNumber[A]): DualNumber[A] = 
            def dAtan(v: A): A = 1 / (v * v + 1)
            chain(trig.atan, dAtan)(a)
        override def atan2(y: DualNumber[A], x: DualNumber[A]): DualNumber[A] = ???
        override def sinh(x: DualNumber[A]): DualNumber[A] = 
            def dSinh(v: A): A = trig.cosh(v)
            chain(trig.sinh, dSinh)(x)
        override def cosh(x: DualNumber[A]): DualNumber[A] =
            def dCosh(v: A): A = trig.sinh(v)
            chain(trig.cosh, dCosh)(x)
        override def tanh(x: DualNumber[A]): DualNumber[A] =
            def dTanh(v: A): A = 1 / (trig.cosh(v) * trig.cosh(v))
            chain(trig.tanh, dTanh)(x)
        override def toRadians(a: DualNumber[A]): DualNumber[A] = 
            def dToRadians(v: A): A = trig.pi / 180
            chain(trig.toRadians, dToRadians)(a)
        override def toDegrees(a: DualNumber[A]): DualNumber[A] =
            def dToDegrees(v: A): A = 180 / trig.pi
            chain(trig.toDegrees, dToDegrees)(a)
 
    /* TODO 
     * Can I somehow implement the super types of numeric and then give a Numeric instance based on them?  
     * Note that I don't want to repeat implementations like times etc.
     * */
/*
    given [T](using r: Ring[T]): Ring[DualNumber[T]] with
        override def zero: DualNumber[T] = DualNumber(r.one, r.zero)
        override def one: DualNumber[T] = DualNumber(r.zero, r.zero)
        override def negate(x: DualNumber[T]): DualNumber[T] = applyBoth(r.negate)(x)
        override def times(x: DualNumber[T], y: DualNumber[T]): DualNumber[T] = 
            def productRule(u: T, du: T, v: T, dv: T): T = 
                du * v + u * dv
            DualNumber[T](x.v * y.v, productRule(x.v, x.dv, y.v, y.dv))
        
        override def plus(x: DualNumber[T], y: DualNumber[T]): DualNumber[T] =
            DualNumber(x.v + y.v, x.dv + y.dv)

    given [T](using ro: NRoot[T], f: Field[T]): NRoot[DualNumber[T]] with
        override def nroot(a: DualNumber[T], n: Int): DualNumber[T] =
            chain(x => ro.nroot(x, n), x => ro.fpow(x, f.one / f.fromInt(n - 1)) / n)(a)
        override def fpow(a: DualNumber[T], b: DualNumber[T]): DualNumber[T] =
            def log(x: T): T = ???  // TODO Trigometric function not part of Numeric :( => add trigometric type class
            // Source: https://www.wolframalpha.com/input?i=f%28x%29%5Eg%28x%29+derivative
            val dfpow = ro.fpow(a.v, b.v - 1) * (b.v * a.dv + a.v * log(a.v) * b.dv)
            DualNumber(ro.fpow(a.v, b.v), dfpow)

    given cfa[A](using am: ConvertableFrom[A]): ConvertableFrom[DualNumber[A]] = 
        DeriverSpireNumeric.createConvertableFrom(_.v)

    given[A](using am: ConvertableTo[A]): ConvertableTo[DualNumber[A]] = 
        DeriverSpireNumeric.createConvertableTo(x => DualNumber(x, am.fromInt(0)))

    given[A](using ir: IsReal[A], ag: AdditiveGroup[A], mm: MultiplicativeMonoid[A]): IsReal[DualNumber[A]] with
        override def ceil(a: DualNumber[A]): DualNumber[A] = 
            def dCeil(v: A): A = 
                // TODO what to do here?
                // f(x) = x - 0.5 approximates the step function of ceil best, which derivative would be 1
                return mm.one
            chain(ir.ceil, dCeil)(a)

        override def floor(a: DualNumber[A]): DualNumber[A] =
            def dFloor(v: A): A = 
                // TODO what to do here?
                // f(x) = x + 0.5 approximates the step function of ceil best, which derivative would be 1
                return mm.one
            chain(ir.floor, dFloor)(a)

        override def round(a: DualNumber[A]): DualNumber[A] =
            def dRound(v: A): A = 
                // TODO what to do here?
                // f(x) = x approximates the step function of ceil best, which derivative would be 1
                return mm.one
            chain(ir.round, dRound)(a)

        override def isWhole(a: DualNumber[A]): Boolean = ir.isWhole(a.v)
        override def toDouble(a: DualNumber[A]): Double = ir.toDouble(a.v)
        override def toReal(a: DualNumber[A]): Real = ir.toReal(a.v)
        override def compare(x: DualNumber[A], y: DualNumber[A]): Int = ir.compare(x.v, y.v)

        override def abs(a: DualNumber[A]): DualNumber[A] =
            def dAbs(v: A): A = 
                if (ir.signum(v) == 0) then
                    ???  // TODO what to do here?
                else
                    ir.sign(v) match
                        case Zero => ag.zero
                        case Positive => mm.one
                        case Negative => ag.negate(mm.one)
                    
            chain(ir.abs, dAbs)(a)

        override def additiveCommutativeMonoid: AdditiveCommutativeMonoid[DualNumber[A]] = ???
        override def order: Order[DualNumber[A]] = new Order {
          override def compare(x: DualNumber[A], y: DualNumber[A]): Int = ir.order.compare(x.v, y.v)
        }
        override def signum(a: DualNumber[A]): Int = ir.signum(a.v)

    given [A](using mag: MultiplicativeAbGroup[A], mm: MultiplicativeMonoid[DualNumber[A]], ag: AdditiveGroup[A]): MultiplicativeAbGroup[DualNumber[A]] with
        override def one: DualNumber[A] = mm.one
        override def times(x: DualNumber[A], y: DualNumber[A]): DualNumber[A] = mm.times(x, y)
        override def reciprocal(a: DualNumber[A]): DualNumber[A] = 
            DualNumber(one.v / a.v, -a.dv / (a.v * a.v))
        override def div(x: DualNumber[A], y: DualNumber[A]): DualNumber[A] =
            def quotientRule(u: A, du: A, v: A, dv: A): A =
                (du * v - u * dv) / (v * v)
            DualNumber[A](x.v / y.v, quotientRule(x.v, x.dv, y.v, y.dv))

    // This breaks implicit search. I think because Numeric is a Ring therefore it loops on itself.
    given[A](using
        ring: Ring[DualNumber[A]],
        additiveAbGroup: AdditiveAbGroup[DualNumber[A]],
        multiplicativeAbGroup: MultiplicativeAbGroup[DualNumber[A]],
        nRoot: NRoot[DualNumber[A]],
        convertableFrom: ConvertableFrom[DualNumber[A]],
        convertableTo: ConvertableTo[DualNumber[A]],
        isReal: IsReal[DualNumber[A]],
    ): Numeric[DualNumber[A]] = 
        new Numeric[DualNumber[A]] {

        // export clause did not work here :(

        override def fromFloat(n: Float): DualNumber[A] = convertableTo.fromFloat(n)

        override def additiveCommutativeMonoid: AdditiveCommutativeMonoid[DualNumber[A]] = isReal.additiveCommutativeMonoid

        override def times(x: DualNumber[A], y: DualNumber[A]): DualNumber[A] = ring.times(x, y)

        override def toNumber(a: DualNumber[A]): Number = convertableFrom.toNumber(a)

        override def order: Order[DualNumber[A]] = ???

        override def toType[B: ConvertableTo](a: DualNumber[A]): B = convertableFrom.toType(a)

        override def toDouble(a: DualNumber[A]): Double = convertableFrom.toDouble(a)

        override def toReal(a: DualNumber[A]): Real = convertableFrom.toReal(a)

        override def negate(x: DualNumber[A]): DualNumber[A] = ring.negate(x)

        override def fromType[B: ConvertableFrom](b: B): DualNumber[A] = convertableTo.fromType(b)

        override def toByte(a: DualNumber[A]): Byte = convertableFrom.toByte(a)

        override def nroot(a: DualNumber[A], n: Int): DualNumber[A] = nRoot.nroot(a, n)

        override def signum(a: DualNumber[A]): Int = isReal.signum(a)

        override def fromBigDecimal(n: BigDecimal): DualNumber[A] = convertableTo.fromBigDecimal(n)

        override def fromAlgebraic(n: Algebraic): DualNumber[A] = convertableTo.fromAlgebraic(n)

        override def one: DualNumber[A] = ring.one

        override def abs(a: DualNumber[A]): DualNumber[A] = ???

        override def fpow(a: DualNumber[A], b: DualNumber[A]): DualNumber[A] = nRoot.fpow(a, b)

        override def compare(x: DualNumber[A], y: DualNumber[A]): Int = ???

        override def plus(x: DualNumber[A], y: DualNumber[A]): DualNumber[A] = ring.plus(x, y)

        override def toRational(a: DualNumber[A]): Rational = convertableFrom.toRational(a)

        override def fromShort(n: Short): DualNumber[A] = convertableTo.fromShort(n)

        override def toFloat(a: DualNumber[A]): Float = convertableFrom.toFloat(a)

        override def toAlgebraic(a: DualNumber[A]): Algebraic = convertableFrom.toAlgebraic(a)

        override def isWhole(a: DualNumber[A]): Boolean = isReal.isWhole(a)

        override def toBigInt(a: DualNumber[A]): BigInt = convertableFrom.toBigInt(a)

        override def fromReal(n: Real): DualNumber[A] = convertableTo.fromReal(n)

        override def toString(a: DualNumber[A]): String = convertableFrom.toString(a)

        override def toShort(a: DualNumber[A]): Short = convertableFrom.toShort(a)

        override def toInt(a: DualNumber[A]): Int = convertableFrom.toInt(a)

        override def zero: DualNumber[A] = ring.zero

        override def fromDouble(n: Double): DualNumber[A] = convertableTo.fromDouble(n)

        override def toBigDecimal(a: DualNumber[A]): BigDecimal = convertableFrom.toBigDecimal(a)

        override def ceil(a: DualNumber[A]): DualNumber[A] = isReal.ceil(a)

        override def div(x: DualNumber[A], y: DualNumber[A]): DualNumber[A] = ???

        override def round(a: DualNumber[A]): DualNumber[A] = isReal.round(a)

        override def fromRational(n: Rational): DualNumber[A] = convertableTo.fromRational(n)

        override def fromLong(n: Long): DualNumber[A] = convertableTo.fromLong(n)

        override def toLong(a: DualNumber[A]): Long = convertableFrom.toLong(a)

        override def floor(a: DualNumber[A]): DualNumber[A] = isReal.floor(a)

        override def fromByte(n: Byte): DualNumber[A] = convertableTo.fromByte(n)

    }
*/

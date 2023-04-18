package scalagrad.spire.api

import scalagrad.api.Deriver
import scala.reflect.ClassTag

import spire.math._
import spire.algebra.Trig

trait Num[A] extends Numeric[A] with Trig[A]

trait DeriverSpireNumeric:

    type DNum[P]

    given spireNumeric[P](using Numeric[P]): Deriver[DNum[P] => DNum[P]]
    given spireNumeric2[P](using Numeric[P]): Deriver[(DNum[P], DNum[P]) => DNum[P]]
    given spireNumericVector[P](using Numeric[P]): Deriver[Vector[DNum[P]] => DNum[P]]

/*
object DeriverSpireNumeric:

    def createConvertableFrom[A, DD[_]](ep: DD[A] => A)(using cf: ConvertableFrom[A]): ConvertableFrom[DD[A]] = 
        new ConvertableFrom[DD[A]] {
            override def toByte(a: DD[A]): Byte = cf.toByte(ep(a))
            override def toShort(a: DD[A]): Short = cf.toShort(ep(a))
            override def toInt(a: DD[A]): Int = cf.toInt(ep(a))
            override def toLong(a: DD[A]): Long = cf.toLong(ep(a))
            override def toFloat(a: DD[A]): Float = cf.toFloat(ep(a))
            override def toDouble(a: DD[A]): Double = cf.toDouble(ep(a))
            override def toBigInt(a: DD[A]): BigInt = cf.toBigInt(ep(a))
            override def toBigDecimal(a: DD[A]): BigDecimal = cf.toBigDecimal(ep(a))
            override def toRational(a: DD[A]): Rational = cf.toRational(ep(a))
            override def toAlgebraic(a: DD[A]): Algebraic = cf.toAlgebraic(ep(a))
            override def toReal(a: DD[A]): Real = cf.toReal(ep(a))
            override def toNumber(a: DD[A]): Number = cf.toNumber(ep(a))
            override def toType[B: ConvertableTo](a: DD[A]): B = cf.toType[B](ep(a))
            override def toString(a: DD[A]): String = a.toString()
        }


    def createConvertableTo[A, DD[_]](lp: A => DD[A])(using ct: ConvertableTo[A]): ConvertableTo[DD[A]] = 
        new ConvertableTo[DD[A]] {
            override def fromByte(n: Byte): DD[A] = lp(ct.fromByte(n))
            override def fromShort(n: Short): DD[A] = lp(ct.fromShort(n))
            override def fromInt(n: Int): DD[A] = lp(ct.fromInt(n))
            override def fromLong(n: Long): DD[A] = lp(ct.fromLong(n))
            override def fromFloat(n: Float): DD[A] = lp(ct.fromFloat(n))
            override def fromDouble(n: Double): DD[A] = lp(ct.fromDouble(n))
            override def fromBigInt(n: BigInt): DD[A] = lp(ct.fromBigInt(n))
            override def fromBigDecimal(n: BigDecimal): DD[A] = lp(ct.fromBigDecimal(n))
            override def fromRational(n: Rational): DD[A] = lp(ct.fromRational(n))
            override def fromAlgebraic(n: Algebraic): DD[A] = lp(ct.fromAlgebraic(n))
            override def fromReal(n: Real): DD[A] = lp(ct.fromReal(n))
            override def fromType[B: ConvertableFrom](b: B): DD[A] = lp(ct.fromType[B](b))
        } 
*/
package scalagrad.forward.dual

import scala.math.Fractional

case class DualNumber[T](value: T, derivative: T):
  inline def v: T = value
  inline def dv: T = derivative

object DualNumber:

  given [T](using f: Fractional[T]): Fractional[DualNumber[T]] with

    import f._
    
    extension (x: DualNumber[T])
      def +(y: DualNumber[T]): DualNumber[T] = DualNumber[T](x.v + y.v, x.dv + y.dv)
      def *(y: DualNumber[T]): DualNumber[T] =
        def productRule(u: T, du: T, v: T, dv: T): T = du * v + u * dv
        DualNumber[T](x.v * y.v, productRule(x.v, x.dv, y.v, y.dv))
      def -(y: DualNumber[T]): DualNumber[T] = DualNumber[T](x.v - y.v, x.dv - y.dv)
      def /(y: DualNumber[T]): DualNumber[T] =
        def quotientRule(u: T, du: T, v: T, dv: T): T =
          (du * v - u * dv) / (v * v)
        DualNumber[T](x.v / y.v, quotientRule(x.v, x.dv, y.v, y.dv))

    override def div(x: DualNumber[T], y: DualNumber[T]): DualNumber[T] = x / y

    override def fromInt(x: Int): DualNumber[T] = DualNumber[T](f.fromInt(x), f.zero)

    override def compare(x: DualNumber[T], y: DualNumber[T]): Int = f.compare(x.v, y.v)

    override def times(x: DualNumber[T], y: DualNumber[T]): DualNumber[T] = x * y

    override def toFloat(x: DualNumber[T]): Float = f.toFloat(x.v)

    override def toInt(x: DualNumber[T]): Int = f.toInt(x.v)

    override def minus(x: DualNumber[T], y: DualNumber[T]): DualNumber[T] = x - y

    override def toLong(x: DualNumber[T]): Long = f.toLong(x.v)

    override def plus(x: DualNumber[T], y: DualNumber[T]): DualNumber[T] = x + y

    override def toDouble(x: DualNumber[T]): Double = f.toDouble(x.v)

    override def parseString(str: String): Option[DualNumber[T]] = None

    override def negate(x: DualNumber[T]): DualNumber[T] = DualNumber[T](f.negate(x.v), f.negate(x.dv))


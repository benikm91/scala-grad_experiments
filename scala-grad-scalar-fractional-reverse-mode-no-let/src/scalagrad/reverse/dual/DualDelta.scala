package scalagrad.reverse.nolet.dual

import scalagrad.reverse.nolet.dual.delta.Delta


case class DualDelta[P](value: P, delta: Delta[P]):
  inline def v: P = value

object DualDelta:

  given [P](using f: Fractional[P]): Fractional[DualDelta[P]] with

    import f._
    
    extension (x: DualDelta[P])
      
      def +(y: DualDelta[P]): DualDelta[P] = DualDelta[P](x.v + y.v, Delta.Add(x.delta, y.delta))
      def *(y: DualDelta[P]): DualDelta[P] =
        def productRule(u: P, du: Delta[P], v: P, dv: Delta[P]): Delta[P] = 
          Delta.Mul(du, Delta.Const[P](v)) + Delta.Mul(dv, Delta.Const[P](u))
        DualDelta[P](x.v * y.v, productRule(x.v, x.delta, y.v, y.delta))
      def -(y: DualDelta[P]): DualDelta[P] = DualDelta[P](x.v - y.v, x.delta - y.delta)
      def /(y: DualDelta[P]): DualDelta[P] =
        def quotientRule(u: P, du: Delta[P], v: P, dv: Delta[P]): Delta[P] =
                  (du * Delta.Const(v) - Delta.Const(u) * dv) / (Delta.Const(v * v))
        DualDelta[P](x.v / y.v, quotientRule(x.v, x.delta, y.v, y.delta))


    override def div(x: DualDelta[P], y: DualDelta[P]): DualDelta[P] = x / y

    override def fromInt(x: Int): DualDelta[P] = DualDelta[P](f.fromInt(x), Delta.Zero)

    override def compare(x: DualDelta[P], y: DualDelta[P]): Int = f.compare(x.v, y.v)

    override def times(x: DualDelta[P], y: DualDelta[P]): DualDelta[P] = x * y

    override def toFloat(x: DualDelta[P]): Float = f.toFloat(x.v)

    override def toInt(x: DualDelta[P]): Int = f.toInt(x.v)

    override def minus(x: DualDelta[P], y: DualDelta[P]): DualDelta[P] = x - y

    override def toLong(x: DualDelta[P]): Long = f.toLong(x.v)

    override def plus(x: DualDelta[P], y: DualDelta[P]): DualDelta[P] = x + y

    override def toDouble(x: DualDelta[P]): Double = f.toDouble(x.v)

    override def parseString(str: String): Option[DualDelta[P]] = None

    override def negate(x: DualDelta[P]): DualDelta[P] = 
      DualDelta[P](
          f.negate(x.v),
          Delta.Mul(x.delta, Delta.Const(f.fromInt(-1)))
        )

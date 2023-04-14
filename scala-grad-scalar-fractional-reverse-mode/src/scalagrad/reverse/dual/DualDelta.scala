package scalagrad.reverse.dual

import scalagrad.reverse.dual.delta.{DeltaMonad, Delta, DeltaId, DeltaState}


case class DualDelta[P](value: P, deltaM: DeltaMonad[P, Delta[P]]):
  inline def v: P = value

object DualDelta:

  def ZeroM[P] = DeltaMonad[P, Delta[P]](state => (state, Delta.Zero))

  def deltaLet[P](delta: Delta[P]): DeltaMonad[P, DeltaId] = DeltaMonad[P, DeltaId](next => 
    next._3.get(delta) match
      case None => (DeltaState(next._1 + 1, (next._1, delta) :: next.bindings, next._3 + (delta -> next._1)), next._1)
      case Some(value) => (next, value)
  )

  given [P](using f: Fractional[P]): Fractional[DualDelta[P]] with

    import f._
    
    extension (x: DualDelta[P])
      
      def +(y: DualDelta[P]): DualDelta[P] = 
        DualDelta[P](
          x.v + y.v,
          for {
            dx <- x.deltaM
            dy <- y.deltaM
            newId <- deltaLet(dx + dy)
          } yield Delta.Val(newId)
        )

      def *(y: DualDelta[P]): DualDelta[P] =
        def productRule(u: P, du: Delta[P], v: P, dv: Delta[P]): Delta[P] = 
          du * Delta.Const(v) + Delta.Const(u) * dv
        DualDelta[P](
          x.v * y.v, 
          for {
            dx <- x.deltaM
            dy <- y.deltaM
            newId <- deltaLet(productRule(x.v, dx, y.v, dy))
          } yield Delta.Val(newId)
        )

      def -(y: DualDelta[P]): DualDelta[P] = 
        DualDelta[P](
          x.v - y.v,
          for {
            dx <- x.deltaM
            dy <- y.deltaM
            newId <- deltaLet(dx - dy)
          } yield Delta.Val(newId)
        )

      def /(y: DualDelta[P]): DualDelta[P] =
        def quotientRule(u: P, du: Delta[P], v: P, dv: Delta[P]): Delta[P] =
          (du * Delta.Const(v) - Delta.Const(u) * dv) / (Delta.Const(v * v))
        DualDelta[P](
          x.v / y.v, 
          for {
            dx <- x.deltaM
            dy <- y.deltaM
            newId <- deltaLet(quotientRule(x.v, dx, y.v, dy))
          } yield Delta.Val(newId)
        )

    override def div(x: DualDelta[P], y: DualDelta[P]): DualDelta[P] = x / y

    override def fromInt(x: Int): DualDelta[P] = DualDelta[P](f.fromInt(x), ZeroM[P])

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
          for {
            dx <- x.deltaM
            newId <- deltaLet(dx * Delta.Const(f.fromInt(-1)))
          } yield Delta.Val(newId)
        )

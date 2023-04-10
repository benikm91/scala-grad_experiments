package scalagrad.reverse.dual

import scalagrad.reverse.delta.{DeltaMonad, Delta, DeltaId, DeltaState}


case class Dual[P](value: P, deltaM: DeltaMonad[P, Delta[P]]):
  inline def v: P = value

object Dual:

  def ZeroM[P] = DeltaMonad[P, Delta[P]](state => (state, Delta.Zero))

  def deltaLet[P](delta: Delta[P]): DeltaMonad[P, DeltaId] = DeltaMonad[P, DeltaId](next => 
    next._3.get(delta) match
      case None => (DeltaState(next._1 + 1, (next._1, delta) :: next.bindings, next._3 + (delta -> next._1)), next._1)
      case Some(value) => (next, value)
  )

  given [P](using f: Fractional[P]): Fractional[Dual[P]] with

    import f._
    
    extension (x: Dual[P])
      
      def +(y: Dual[P]): Dual[P] = 
        Dual[P](
          x.v + y.v,
          for {
            dx <- x.deltaM
            dy <- y.deltaM
            newId <- deltaLet(dx + dy)
          } yield Delta.Val(newId)
        )

      def *(y: Dual[P]): Dual[P] =
        def productRule(u: P, du: Delta[P], v: P, dv: Delta[P]): Delta[P] = 
          du * Delta.Const(v) + Delta.Const(u) * dv
        Dual[P](
          x.v * y.v, 
          for {
            dx <- x.deltaM
            dy <- y.deltaM
            newId <- deltaLet(productRule(x.v, dx, y.v, dy))
          } yield Delta.Val(newId)
        )

      def -(y: Dual[P]): Dual[P] = 
        Dual[P](
          x.v - y.v,
          for {
            dx <- x.deltaM
            dy <- y.deltaM
            newId <- deltaLet(dx - dy)
          } yield Delta.Val(newId)
        )

      def /(y: Dual[P]): Dual[P] =
        def quotientRule(u: P, du: Delta[P], v: P, dv: Delta[P]): Delta[P] =
          (du * Delta.Const(v) - Delta.Const(u) * dv) / (Delta.Const(v * v))
        Dual[P](
          x.v / y.v, 
          for {
            dx <- x.deltaM
            dy <- y.deltaM
            newId <- deltaLet(quotientRule(x.v, dx, y.v, dy))
          } yield Delta.Val(newId)
        )

    override def div(x: Dual[P], y: Dual[P]): Dual[P] = x / y

    override def fromInt(x: Int): Dual[P] = Dual[P](f.fromInt(x), ZeroM[P])

    override def compare(x: Dual[P], y: Dual[P]): Int = f.compare(x.v, y.v)

    override def times(x: Dual[P], y: Dual[P]): Dual[P] = x * y

    override def toFloat(x: Dual[P]): Float = f.toFloat(x.v)

    override def toInt(x: Dual[P]): Int = f.toInt(x.v)

    override def minus(x: Dual[P], y: Dual[P]): Dual[P] = x - y

    override def toLong(x: Dual[P]): Long = f.toLong(x.v)

    override def plus(x: Dual[P], y: Dual[P]): Dual[P] = x + y

    override def toDouble(x: Dual[P]): Double = f.toDouble(x.v)

    override def parseString(str: String): Option[Dual[P]] = None

    override def negate(x: Dual[P]): Dual[P] = 
      Dual[P](
          f.negate(x.v),
          for {
            dx <- x.deltaM
            newId <- deltaLet(dx * Delta.Const(f.fromInt(-1)))
          } yield Delta.Val(newId)
        )

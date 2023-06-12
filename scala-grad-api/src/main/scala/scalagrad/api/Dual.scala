package scalagrad.api

import scala.math.Fractional.Implicits.given


trait Dual[P, D, PD <: Dual[P, D, PD]](using f: Fractional[P]):

    def v: P
    def dv: D

    val cd: CreateDual[P, D, PD]
    import cd.*

    def addD(d1: D, d2: D): D
    def subD(d1: D, d2: D): D
    def scale(d: D, p: P): D
    def inverseScale(d: D, p: P): D

    extension (d1: D)
        def |+|(d2: D): D = addD(d1, d2)
        def |*|(p: P): D = scale(d1, p)
        def |/|(p: P): D = inverseScale(d1, p)
        def |-|(d2: D): D = subD(d1, d2)

    def +(y: PD): PD =
        val x = this
        create(
            x.v + y.v, 
            x.dv |+| y.dv
        )

    def *(y: PD): PD =
        val x = this
        def productRule(u: P, du: D, v: P, dv: D): D = 
            (du |*| v) |+| (dv |*| u)
        create(x.v * y.v, productRule(x.v, x.dv, y.v, y.dv))
    
    def -(y: PD): PD =
        val x = this
        create(x.v - y.v, x.dv |-| y.dv)

    def /(y: PD): PD =
        val x = this
        def quotientRule(u: P, du: D, v: P, dv: D): D =
            ((du |*| v) |-| (dv |*| u)) |/| (v * v)
        create(x.v / y.v, quotientRule(x.v, x.dv, y.v, y.dv))

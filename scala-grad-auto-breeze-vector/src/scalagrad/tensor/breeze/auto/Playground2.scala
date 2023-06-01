package scalagrad.tensor.breeze.auto

import scalagrad.auto.forward.dual.DualNumber
import math.Fractional.Implicits.infixFractionalOps

trait VectorSpace[P: Fractional, Vector, Scalar]:

    def elementWiseOp(v: Vector, op: Scalar => Scalar): Vector
    def dot(v1: Vector, v2: Vector): Scalar
    def plus(v1: Vector, v2: Vector): Vector


object VectorSpace:

    given scalaVector: VectorSpace[Double, Vector[Double], Double] with
        override def dot(v1: Vector[Double], v2: Vector[Double]): Double =
            v1.zip(v2).map((x, y) => x * y).sum

    given breezeVector: VectorSpace[Double, breeze.linalg.Vector[Double], Double] with
        override def dot(v1: breeze.linalg.Vector[Double], v2: breeze.linalg.Vector[Double]): Double =
            v1.dot(v2)

    given dualVector[P: Fractional](using vops: VectorSpace[P, Vector[P], P]): VectorSpace[P, DualVector[P], DualNumber[P]] with
        override def dot(v1: DualVector[P], v2: DualVector[P]): DualNumber[P] =
            def dDot(u: Vector[P], du: Vector[P], v: Vector[P], dv: Vector[P]): P = 
                vops.dot(u, dv) + vops.dot(du, v)
            DualNumber(
                vops.dot(v1.a, v2.a),
                dDot(v1.a, v1.b, v2.a, v2.b)
            )


case class DualVector[P](a: Vector[P], b: Vector[P])


object Playground2:

    @main
    def playground2():
        import spire.algebra.Field
        import spire.algebra.VectorSpace
        import spire.implicits._

        Vector(1,5,3) + Vector(2,1,-5)
        //Result is Vector(3, 6, -2)

        4.0 *: Vector(1.0,5.0,3.0)
        //Result is Vector(4.0, 20.0, 12.0)

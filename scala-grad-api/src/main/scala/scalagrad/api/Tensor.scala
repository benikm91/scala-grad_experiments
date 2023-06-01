package scalagrad.api

import scala.annotation.targetName
import scala.math.Fractional.Implicits._


trait VectorAlgebraOps:

    type Scalar
    type ColumnVector
    type RowVector
    type Matrix

    def liftToScalar(d: Int): Scalar

    def transpose(m: Matrix): Matrix
    def transposeColumVector(m: ColumnVector): RowVector
    def transposeRowVector(m: RowVector): ColumnVector
    // def inverse(m: Matrix): Matrix

    def timesMM(m1: Matrix, m2: Matrix): Matrix
    def timesVV(v1: RowVector, v2: ColumnVector): Scalar
    def timesOuterVV(v1: ColumnVector, v2: RowVector): Matrix
    def timesVM(v: RowVector, m: Matrix): RowVector
    def timesMV(m: Matrix, v: ColumnVector): ColumnVector
    def timesVS(v: ColumnVector, s: Scalar): ColumnVector
    def timesSV(s: Scalar, v: ColumnVector): ColumnVector = timesVS(v, s)
    def timesMS(m: Matrix, s: Scalar): Matrix
    def timesSM(s: Scalar, m: Matrix): Matrix = timesMS(m, s)
    def timesSS(s1: Scalar, s2: Scalar): Scalar

    def plusMM(m1: Matrix, m2: Matrix): Matrix
    def plusVV(v1: ColumnVector, v2: ColumnVector): ColumnVector
    def plusMV(m: Matrix, v: ColumnVector): Matrix
    def plusVM(v: ColumnVector, m: Matrix): Matrix = plusMV(m, v)
    def plusVS(v: ColumnVector, s: Scalar): ColumnVector
    def plusSV(s: Scalar, v: ColumnVector): ColumnVector = plusVS(v, s)
    def plusMS(m: Matrix, s: Scalar): Matrix
    def plusSM(s: Scalar, m: Matrix): Matrix = plusMS(m, s)
    def plusSS(s1: Scalar, s2: Scalar): Scalar

    def minusMM(m1: Matrix, m2: Matrix): Matrix
    def minusVV(v1: ColumnVector, v2: ColumnVector): ColumnVector
    def minusMV(m: Matrix, v: ColumnVector): Matrix 
    def minusVM(v: ColumnVector, m: Matrix): Matrix
    def minusVS(v: ColumnVector, s: Scalar): ColumnVector
    def minusSV(s: Scalar, v: ColumnVector): ColumnVector = minusVS(v, s)
    def minusMS(m: Matrix, s: Scalar): Matrix
    def minusSM(s: Scalar, m: Matrix): Matrix = minusMS(m, s)
    def minusSS(s1: Scalar, s2: Scalar): Scalar

    def elementWiseOps(v: ColumnVector, f: [T] => T => Numeric[T] ?=> T): ColumnVector
    def elementWiseOpsM(v: Matrix, f: [T] => T => Numeric[T] ?=> T): Matrix

    def sum(v: ColumnVector): Scalar
    def length(v: ColumnVector): Int

    def divideSS(s1: Scalar, s2: Scalar): Scalar

    extension (m1: Matrix)
        def *(m2: Matrix): Matrix = timesMM(m1, m2)
        def +(m2: Matrix): Matrix = plusMM(m1, m2)
        def -(m2: Matrix): Matrix = minusMM(m1, m2)

    extension (v: RowVector)
        @targetName("timesVM_Op")
        def *(m: Matrix): RowVector = timesVM(v, m)
        @targetName("timesVV_Op")
        def *(v2: ColumnVector): Scalar = timesVV(v, v2)
    
    extension (v: ColumnVector)
        @targetName("timesVS_Op")
        def *(s: Scalar): ColumnVector = timesVS(v, s)
        @targetName("timesOuterVV_Op")
        def *(v2: RowVector): Matrix = timesOuterVV(v, v2)
        @targetName("plusVM_Op")
        def +(m: Matrix): Matrix = plusVM(v, m)
        @targetName("plusVV_Op")
        def +(v2: ColumnVector): ColumnVector = plusVV(v, v2)
        @targetName("plusVS_Op")
        def +(s: Scalar): ColumnVector = plusVS(v, s)
        @targetName("minusVM_Op")
        def -(m: Matrix): Matrix = minusVM(v, m)
        @targetName("minusVV_Op")
        def -(v2: ColumnVector): ColumnVector = minusVV(v, v2)
        @targetName("minusVS_Op")
        def -(s: Scalar): ColumnVector = minusVS(v, s)

    extension (m: Matrix)
        @targetName("timesMV_Op")
        def *(v: ColumnVector): ColumnVector = timesMV(m, v)
        @targetName("timesMS_Op")
        def *(s: Scalar): Matrix = timesMS(m, s)
        @targetName("plusMV_Op")
        def +(v: ColumnVector): Matrix = plusMV(m, v)
        @targetName("plusMS_Op")
        def +(s: Scalar): Matrix = plusMS(m, s)
        @targetName("minusMV_Op")
        def -(v: ColumnVector): Matrix = minusMV(m, v)
        @targetName("minusMS_Op")
        def -(s: Scalar): Matrix = minusMS(m, s)

    extension (s: Scalar)
        @targetName("timesSM_Op")
        def *(m: Matrix): Matrix = timesSM(s, m)
        @targetName("timesSV_Op")
        def *(v: ColumnVector): ColumnVector = timesSV(s, v)
        @targetName("timesSS_Op")
        def *(s2: Scalar): Scalar = timesSS(s, s2)
        @targetName("plusSM_Op")
        def +(m: Matrix): Matrix = plusSM(s, m)
        @targetName("plusSV_Op")
        def +(v: ColumnVector): ColumnVector = plusSV(s, v)
        @targetName("plusSS_Op")
        def +(s2: Scalar): Scalar = plusSS(s, s2)
        @targetName("minusSM_Op")
        def -(m: Matrix): Matrix = minusSM(s, m)
        @targetName("minusSV_Op")
        def -(v: ColumnVector): ColumnVector = minusSV(s, v)
        @targetName("minusSS_Op")
        def -(s2: Scalar): Scalar = minusSS(s, s2)
        @targetName("divideSS_Op")
        def /(s2: Scalar): Scalar = divideSS(s, s2)
        
    

class VectorAlgebraFor[P: Fractional]() extends VectorAlgebraOps:

    override type Scalar = P
    override type ColumnVector = Vector[P]
    override type RowVector = Vector[P]
    override type Matrix = Vector[Vector[P]]

    def liftToScalar(i: Int): Scalar = summon[Fractional[P]].fromInt(i)

    override def timesSS(s1: Scalar, s2: Scalar): Scalar = 
        summon[Fractional[P]].times(s1, s2)

    override def timesMS(m: Vector[Vector[P]], s: P): Matrix =
        m.map(row => row.map(_ * s))

    override def timesMV(m: Vector[Vector[P]], v: Vector[P]): ColumnVector =
        m.map(row => row.zip(v).map((x, y) => x * y).sum)

    override def plusMV(m: Vector[Vector[P]], v: Vector[P]): Matrix =
        m.zip(v).map((row, s) => row.map(_ + s))

    override def minusVS(v: Vector[P], s: P): ColumnVector =
        v.map(_ - s)

    override def minusSS(s1: P, s2: P): Scalar = 
        summon[Fractional[P]].minus(s1, s2)

    override def timesVM(v: Vector[P], m: Vector[Vector[P]]): ColumnVector =
        transpose(m).map(row => row.zip(v).map((x, y) => x * y).sum)

    override def minusMM(m1: Vector[Vector[P]], m2: Vector[Vector[P]]): Matrix =
        m1.zip(m2).map((row1, row2) => row1.zip(row2).map((x, y) => x - y))

    override def minusVM(v: Vector[P], m: Vector[Vector[P]]): Matrix =
        transpose(m).map(row => row.zip(v).map((x, y) => x - y))

    override def minusMV(m: Vector[Vector[P]], v: Vector[P]): Matrix =
        m.zip(v).map((row, s) => row.map(_ - s))

    override def minusVV(v1: Vector[P], v2: Vector[P]): ColumnVector =
        v1.zip(v2).map((x, y) => x - y)

    override def timesVV(v1: Vector[P], v2: Vector[P]): Scalar =
        v1.zip(v2).map((x, y) => x * y).sum

    override def timesOuterVV(v1: Vector[P], v2: Vector[P]): Matrix = ???

    override def timesVS(v: Vector[P], s: P): ColumnVector = 
        v.map(_ * s)

    override def plusMS(m: Vector[Vector[P]], s: P): Matrix = 
        m.map(row => row.map(_ + s))

    override def plusVS(v: Vector[P], s: P): ColumnVector = 
        v.map(_ + s)

    override def plusVV(v1: Vector[P], v2: Vector[P]): ColumnVector =
        v1.zip(v2).map((x, y) => x + y)

    override def minusMS(m: Vector[Vector[P]], s: P): Matrix =
        m.map(row => row.map(_ - s))


    override def plusMM(m1: Vector[Vector[P]], m2: Vector[Vector[P]]): Matrix =
        m1.zip(m2).map((row1, row2) => row1.zip(row2).map((x, y) => x + y))

    override def timesMM(m1: Vector[Vector[P]], m2: Vector[Vector[P]]): Matrix =
        val m2t = transpose(m2)
        for (row1 <- m1) yield
            for (row2 <- m2t) yield
                row1.zip(row2).map((x, y) => x * y).sum

    override def plusSS(s1: P, s2: P): Scalar = 
        summon[Fractional[P]].plus(s1, s2)

    def transpose(m: Matrix): Matrix = 
        val n = m.size
        val m0 = m(0)
        (for (i <- 0 until n) yield
            (for (j <- 0 until n) yield m(j)(i)).toVector
        ).toVector

    def transposeColumVector(m: ColumnVector): RowVector = m
    def transposeRowVector(m: RowVector): ColumnVector = m

    def elementWiseOps(v: Vector[P], f: [T] => T => Numeric[T] ?=> T): ColumnVector = 
        v.map(f[P])
    
    def elementWiseOpsM(v: Vector[Vector[P]], f: [T] => (x: T) => (Numeric[T]) ?=> T): Matrix =
        v.map(row => row.map(f[P]))

    def sum(v: ColumnVector): Scalar = v.sum

    def length(v: ColumnVector): Int = v.length

    def divideSS(s1: Scalar, s2: Scalar) = 
        summon[Fractional[P]].div(s1, s2)
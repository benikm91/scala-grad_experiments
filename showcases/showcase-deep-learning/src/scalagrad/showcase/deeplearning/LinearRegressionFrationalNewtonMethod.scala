package scalagrad.showcase.deeplearning

import scalagrad.api.ScalaGrad

import scalagrad.fractional.auto.dual.DualIsFractional.given


import scalagrad.showcase.deeplearning.Util.*

import scalagrad.fractional.auto.dual.DualIsFractional

import scalagrad.auto.forward.DeriverForwardPlan


@main def linearRegressionNewtonMethodAutoDiff() = 

    val fishs = FishDataSet.load

    val xsRaw = fishs.map(fish => Vector(fish.length1, fish.length2, fish.length3, fish.height, fish.width)).toVector
    def generatePolynomialFeatures(x: Vector[Double], degree: Int): Vector[Double] =
        if degree == 1 then x
        else
            generatePolynomialFeatures(x ++ x.zip(x).map { case (x1, x2) => x1 * x2 }, degree = degree - 1)
    val xsPolynom = xsRaw.map(generatePolynomialFeatures(_, 3))
    val xs = xsRaw
    // val xs = xsPolynom
    val ys = fishs.map(_.weight).toVector
    println(f"Samples: ${xs.size}, Features ${xs(0).size}")

    val (xs_ss, _, _) = StandardScaler.scaleMatrix(xs)
    val (ys_ss, ys_mean, ys_std) = StandardScaler.scaleColumn(ys)

    def predict[T](x: Vector[T], w0: T, ws: Vector[T])(using f: Fractional[T]): T = 
        import f.*
        w0 + x.zip(ws).map(_ * _).sum

    def loss[T](ys: Vector[T], ysHat: Vector[T])(using f: Fractional[T]): T =
        import f.*
        ys.zip(ysHat).map { case (y, yHat) => 
            (y - yHat) * (y - yHat)
        }.sum / fromInt(ys.size * 2)

    def lossF[T: Fractional](xs: Vector[Vector[T]], ys: Vector[T])(w0Andws: Vector[T]): T =
        val w0 = w0Andws.head
        val ws = w0Andws.tail
        loss(ys, xs.map(predict(_, w0, ws)))

    def newtonMethodF(w0: Double, ws: Vector[Double])(dLoss: Vector[Double] => Vector[Double], d2Loss: Vector[Double] => Vector[Vector[Double]]): (Double, Vector[Double]) =
        def inverse(m: Vector[Vector[Double]]): Vector[Vector[Double]] = 
            import breeze.linalg._
            val bm = DenseMatrix(m.map(_.toArray): _*)
            inv(bm).toArray.grouped(bm.cols).toVector.map(_.toVector)
        def dot(m: Vector[Vector[Double]], v: Vector[Double]): Vector[Double] = 
            import breeze.linalg._
            val bm = DenseMatrix(m.map(_.toArray): _*)
            val bv = DenseVector(v.toArray)
            (bm * bv).toArray.toVector
        val w0AndWs = Vector(w0) ++ ws
        val a = dLoss(w0AndWs)
        val b = d2Loss(w0AndWs)
        val res = w0AndWs.zip(dot(inverse(b), a)).map(_ - _)
        (res.head, res.tail)

        

    val (initW0, initWs) = (0.0, Vector.fill(xs(0).size)(0.0))
    
    val initYsHat = StandardScaler.inverseScaleColumn(xs_ss.map(predict(_, initW0, initWs)), ys_mean, ys_std)
    println(f"${Math.sqrt(loss(ys, initYsHat))}g  -- RMSE with initial weights")
    val newtonMethod = newtonMethodF(initW0, initWs)
    time {
        import scalagrad.auto.forward.dual.DualNumber
        import scalagrad.auto.forward.dual.DualNumber.given
        import scalagrad.auto.forward.DeriverForwardPlan.given

        println("Forward mode")
        val dLoss = ScalaGrad.derive(lossF[DualNumber[Double]](
            xs_ss.map(_.map(DualNumber(_, 0.0))), 
            ys_ss.map(DualNumber(_, 0.0)
        )))
        val d2Loss = ScalaGrad.derive(ScalaGrad.derive(lossF[DualNumber[DualNumber[Double]]](
            xs_ss.map(_.map(x => DualNumber(DualNumber(x, 0.0), DualNumber(0.0, 0.0)))), 
            ys_ss.map(x => DualNumber(DualNumber(x, 0.0), DualNumber(0.0, 0.0)))
        )))
        val (w0, ws) = newtonMethod(dLoss, d2Loss)
        val ysHat = StandardScaler.inverseScaleColumn(xs_ss.map(predict(_, w0, ws)), ys_mean, ys_std)
        println(f"${Math.sqrt(loss(ys, ysHat))}g  -- RMSE with learned weights")
    }

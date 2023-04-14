package scalagrad.forward

/*

import breeze.linalg.DenseMatrix
import breeze.math.Field
import breeze.linalg.DenseVector
import breeze.numerics.sigmoid
import breeze.generic.UFunc
import breeze.generic.MappingUFunc
import breeze.linalg._
import scala.reflect.ClassTag
import breeze.numerics.log
import scalagrad.api.ScalaGrad.derive
import scalagrad.api.Deriver
import breeze.linalg.operators.OpMulScalar

case class LogisticRegression[P: Field](weights: DenseVector[P]):

    given sigmoidImplFractional[P](using f: Field[P], exp: breeze.numerics.exp.Impl[P, P]): breeze.numerics.sigmoid.Impl[P, P] with
        def apply(x: P): P = 
            f./(f.one, f.+(f.one, exp(f.negate(x))))

    def predict(data: DenseMatrix[P])(using sig: breeze.numerics.sigmoid.Impl[DenseVector[P], DenseVector[P]]): DenseVector[P] = 
        val logits = data * weights
        sigmoid(logits)


object LogisticRegression:

    def costF[P: ClassTag]
        (xs: DenseMatrix[P], ys: DenseVector[Boolean])
        (using 
        f: Field[P], 
        sig: breeze.numerics.sigmoid.Impl[DenseVector[P], DenseVector[P]], 
        l: breeze.numerics.log.Impl[P, P],
        o: OpMulScalar.Impl2[P, DenseVector[P], P],
        )
        (params: DenseVector[P]): P = 
        val ys_hat = LogisticRegression(params).predict(xs)
        val ys_p = ys.map(x => x match
            case true => f.one
            case false => f.zero
        )
        sum(ys_hat(ys).map(y_hat => f.negate(log(y_hat))) + ys_hat(ys).map(y_hat => f.negate(f.one - log(y_hat))))

    def fit[P: ClassTag]
    (
        xs: DenseMatrix[P], ys: DenseVector[Boolean], alpha: P, epsilon: P
    )(
        using 
        f: Field[P], 
        dnf: Field[DualNumber[P]], 
        sig: breeze.numerics.sigmoid.Impl[DenseVector[P], DenseVector[P]], 
        l: breeze.numerics.log.Impl[P, P],
        o: OpMulScalar.Impl2[P, DenseVector[P], P],
        deriver: Deriver[DenseVector[DualNumber[P]] => DualNumber[P]] {
            type dfInput = DenseVector[P]
            type dfOutput = DenseVector[P]
        }
    ) =
        var currentParams = DenseVector.zeros(xs.cols)
        val loss = costF(xs.map(x => DualNumber[P](x, f.zero)), ys)
        val dLoss = derive(loss)

        for (i <- 0 until 100)
            val dParams = dLoss(currentParams)
            currentParams -= alpha *:* dParams
        
        currentParams


object DoIt:
    def doIt() =
        LogisticRegression.fit[DualNumber[Double]]

*/
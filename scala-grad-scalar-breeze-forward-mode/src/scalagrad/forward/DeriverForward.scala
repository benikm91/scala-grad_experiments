package scalagrad.forward

import scalagrad.api.Deriver
import scalagrad.api.DeriverWithPrecision
import scala.reflect.ClassTag
import scala.math.Fractional
import scala.runtime.Tuples

import scalagrad.forward.DeriverForward
import scalagrad.forward.DualNumber
import scalagrad.forward.api.DeriverBreeze
import breeze.linalg.DenseVector
import breeze.math.Field


object DeriverBreezeForward extends DeriverBreeze:

    type DNum[V] = DualNumber[V]

    given breezeDenseVectorToScalar[P : ClassTag] (using frac: Field[P]): DeriverForward[DenseVector[DNum[P]] => DNum[P]] with
        override type dfInput = DenseVector[P]
        override type dfOutput = DenseVector[P]
        override def derive(f: fT): dfT = 
            (x) =>
                val res = for (ok <- x.keysIterator)
                    yield f(
                        x.mapPairs((ik, v) => DualNumber[P](v, if ik == ok then frac.one else frac.zero))
                    ).dv
                DenseVector[P](res.toArray[P])

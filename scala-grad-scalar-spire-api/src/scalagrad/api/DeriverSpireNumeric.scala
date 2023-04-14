package scalagrad.api

import scalagrad.api.Deriver
import scala.reflect.ClassTag

import spire.math.Numeric

trait DeriverSpireNumeric:

    type DNum[P]

    given spireNumeric[P](using Numeric[P]): Deriver[DNum[P] => DNum[P]]
    given spireNumeric2[P](using Numeric[P]): Deriver[(DNum[P], DNum[P]) => DNum[P]]
    given spireNumericVector[P](using Numeric[P]): Deriver[Vector[DNum[P]] => DNum[P]]
    
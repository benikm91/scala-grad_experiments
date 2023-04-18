package scalagrad.fractional.api

import scalagrad.api.Deriver
import scala.reflect.ClassTag

trait DeriverFractional:

    type DNum[P]

    given fractional[P](using Fractional[P]): Deriver[DNum[P] => DNum[P]]
    given fractional2[P](using Fractional[P]): Deriver[(DNum[P], DNum[P]) => DNum[P]]
    given fractionalVector[P](using Fractional[P]): Deriver[Vector[DNum[P]] => DNum[P]]
    
    // given fractionalArray[P: ClassTag](using Fractional[P]): Deriver[Array[DNum[P]] => DNum[P]]
    
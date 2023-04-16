package scalagrad.api

trait Deriver[fT2]:

    type fT = fT2

    type dfInput
    type dfOutput

    type dfT = dfInput => dfOutput

    def derive(f: fT): (dfT)

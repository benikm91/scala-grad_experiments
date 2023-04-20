package scalagrad.api


trait CreateDual[P, D, PD <: Dual[P, D, PD]]:

    def create(p: P, d: D): PD
    def createEmpty(p: P): PD

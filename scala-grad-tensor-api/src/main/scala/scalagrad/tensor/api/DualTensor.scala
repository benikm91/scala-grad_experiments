package scalagrad.tensor.api

import scalagrad.api.Dual
import scalagrad.api.DeriverFromTo

trait DualVector[
    Value, 
    ValueVector[_],
    ValueMatrix[_],
    Derivative, 
    DerivativeVector,
    DerivativeMatrix,
    DS <: Dual[Value, Derivative, DS], 
    DV <: DualVector[Value, ValueVector, ValueMatrix, Derivative, DerivativeVector, DerivativeMatrix, DS, DV, DM],
    DM <: DualMatrix[Value, ValueVector, ValueMatrix, Derivative, DerivativeVector, DerivativeMatrix, DS, DV, DM]
]:

    def value: ValueVector[Value]
    def derivative: DerivativeVector

    def length: Int

    def +(that: DV): DV
    def -(that: DV): DV
    def dot(that: DV): DS
    def outerDot(that: DV): DM

    def +(that: DS): DV
    def *(value: Value): DV
    def *(that: DS): DV

    // TODO can we move df outside?
    def map(f: Value => Value, df: Derivative => Derivative): DV
    def sum: DS // TODO: should this be in DV?

trait DualMatrix[
    Value, 
    ValueVector[_], 
    ValueMatrix[_], 
    Derivative,
    DerivativeVector,
    DerivativeMatrix,
    DS <: Dual[Value, Derivative, DS], 
    DV <: DualVector[Value, ValueVector, ValueMatrix, Derivative, DerivativeVector, DerivativeMatrix, DS, DV, DM],
    DM <: DualMatrix[Value, ValueVector, ValueMatrix, Derivative, DerivativeVector, DerivativeMatrix, DS, DV, DM]
]:
  def value: ValueMatrix[Value]
  def derivative: DerivativeMatrix
  
  def +(that: DS): DM
  def +(that: DM): DM
  def -(that: DM): DM
  def *(that: DM): DM
  def *(that: DV): DV
  def *(that: DS): DM


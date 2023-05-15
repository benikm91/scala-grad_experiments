package scalagrad.showcase.probabilisticProgramming.metropolisHastings.proposer

trait Proposer[Sample]:
    def nextProposal(x: Sample): Sample
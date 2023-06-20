package scalagrad.linearalgebra.auto.reverse.delta.visualize

import scalagrad.linearalgebra.auto.reverse.delta.*
import scala.deriving.Mirror

object DeltaToGraphviz {

  private def graphVizHeader: String = 
    val sb = new StringBuilder
    sb.append("digraph G {\n")
    sb.append("  rankdir=LR;\n")
    sb.append("  node [shape=record];\n")
    sb.append("  edge [arrowhead=none];\n")
    sb.toString

  private def graphVizFooter: String = "}\n"

  def toGraphViz[P](d: Deltas[P]): String = 
    val sb = new StringBuilder
    sb.append(graphVizHeader)
    sb.append(encodeDelta(d))
    sb.append(graphVizFooter)
    sb.toString

  def encodeDelta[P](d: Deltas[P]): String = ???

  def countByType[P](d: Deltas[P]): Map[String, Int] =
    DeltaTraverse.traverse(d).toSet.toList.map(_.getClass.getSimpleName).groupBy(identity).view.mapValues(_.size).toMap

}

package com.mkobiers.med
package domain

import scala.math.BigDecimal.RoundingMode

case class Rule(
    predecessor: ItemSet,
    successor: ItemSet,
    support: Support,
    confidence: Confidence,
    metrics: Vector[Metric]
)

object Rule {
  def text(rule: Rule): String = {
    val predecessors = rule.predecessor.map(_.value).mkString(", ")
    val successors   = rule.successor.map(_.value).mkString(", ")
    val metrics = if (rule.metrics.nonEmpty) {
      rule.metrics
        .map(m => s"${m.getClass.getSimpleName}: ${Metric.text(m)}")
        .mkString("\n\t", "\n\t", "")
    } else {
      "none"
    }
    s"""Rule $predecessors -> $successors
       | support=${rule.support.txs.size}
       | confidence=${rule.confidence.value.setScale(3, RoundingMode.DOWN)}
       | metrics: $metrics""".stripMargin
  }
}

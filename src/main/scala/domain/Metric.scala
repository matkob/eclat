package com.mkobiers.med
package domain

import scala.math.BigDecimal.RoundingMode
import scala.util.Try

sealed trait Metric {
  def value(): Option[BigDecimal]
}

case class LiftIndex(
    ruleConfidence: Confidence,
    successorProbability: BigDecimal
) extends Metric {
  override val value: Option[BigDecimal] =
    Try(ruleConfidence.value / successorProbability).toOption
}

case class CertaintyFactor(
    ruleConfidence: Confidence,
    successorProbability: BigDecimal
) extends Metric {
  override val value: Option[BigDecimal] = ruleConfidence match {
    case higher if higher.value > successorProbability =>
      Try(
        (higher.value - successorProbability) / (1 - successorProbability)
      ).toOption
    case lower if lower.value < successorProbability =>
      Try(-(successorProbability - lower.value) / successorProbability).toOption
    case _ =>
      Some(BigDecimal(0))
  }
}

case class OddsRatio(
    xy: BigDecimal,
    xNotY: BigDecimal,
    notXY: BigDecimal,
    notXNotY: BigDecimal
) extends Metric {
  override val value: Option[BigDecimal] = Try(
    xy * notXNotY / notXY / xNotY
  ).toOption
}

case class JaccardIndex(
    predecessorSupport: Support,
    successorSupport: Support
) extends Metric {
  override val value: Option[BigDecimal] = {
    val intersection = predecessorSupport.txs intersect successorSupport.txs
    val union        = predecessorSupport.txs ++ successorSupport.txs
    Try(BigDecimal(intersection.size) / BigDecimal(union.size)).toOption
  }
}

object Metric {
  def calculate(
      predecessor: ItemSet,
      successor: ItemSet,
      confidence: Confidence,
      itemSetMeta: Map[ItemSet, Support],
      transactionIds: Set[TransactionId]
  ): Vector[Metric] = {
    val predecessorSupport = itemSetMeta(predecessor)
    val successorSupport   = itemSetMeta(successor)
    val successorProbability =
      BigDecimal(successorSupport.txs.size) / BigDecimal(transactionIds.size)

    val lift =
      LiftIndex(confidence, successorProbability)
    val certainty = CertaintyFactor(
      confidence,
      successorProbability
    )
    val odds = OddsRatio(
      (predecessorSupport.txs ++ successorSupport.txs).size,
      (predecessorSupport.txs ++ transactionIds.diff(
        successorSupport.txs
      )).size,
      (transactionIds.diff(
        predecessorSupport.txs
      ) ++ successorSupport.txs).size,
      transactionIds.diff(predecessorSupport.txs ++ successorSupport.txs).size
    )
    val jaccard = JaccardIndex(predecessorSupport, successorSupport)
    Vector(lift, certainty, odds, jaccard)
  }

  def text(metric: Metric): String = {
    metric
      .value()
      .fold("could not calculate")(_.setScale(3, RoundingMode.DOWN).toString())
  }
}

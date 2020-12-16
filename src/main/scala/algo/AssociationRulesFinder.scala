package com.mkobiers.med
package algo

import domain._

trait AssociationRulesFinder {
  def associationRules(
      transactions: Vector[Transaction],
      minSupport: MinSupport,
      minConfidence: MinConfidence
  ): Map[RuleId, Rule]

  def frequentItemSets(
      transactions: Vector[Transaction],
      minSupport: MinSupport
  ): Map[ItemSet, Support]
}

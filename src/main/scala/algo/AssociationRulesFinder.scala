package com.mkobiers.med
package algo

import domain._

trait AssociationRulesFinder {
  def associationRules(
      transactions: Vector[Transaction],
      minSupport: MinSupport
  ): Either[Throwable, Vector[Rule]]

  def frequentItemSets(
      transactions: Vector[Transaction],
      minSupport: MinSupport
  ): Either[Throwable, Map[ItemSet, Support]]
}

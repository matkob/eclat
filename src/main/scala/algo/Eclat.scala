package com.mkobiers.med
package algo

import domain._

trait AssociationFinder {
  def associationRules(transactions: Vector[Transaction], minSupport: Support): Either[Throwable, Vector[Rule]]
  def frequentItemSets(transactions: Vector[Transaction], minSupport: Support): Either[Throwable, Map[ItemSet, Support]]
}

object Eclat extends AssociationFinder {
  case class ItemSetMetaData(itemSet: ItemSet, support: Support, predecessor: ItemSet, successors: Set[ItemSet] = Set.empty[ItemSet])

  type ItemSetGenerators = Vector[ItemSet]
  type ItemSetMeta = Map[ItemSet, ItemSetMetaData]

  override def associationRules(transactions: Vector[Transaction], minSupport: Support): Either[Throwable, Vector[Rule]] = ???

  override def frequentItemSets(transactions: Vector[Transaction], minSupport: Support): Either[Throwable, Map[ItemSet, Support]] = {
    val itemSetMeta: ItemSetMeta = frequentItems(transactions, minSupport)
    val generators: ItemSetGenerators = itemSetMeta.keys.toVector
    val biggestTx = transactions.map(_.items.size).max
    val _, support = (1 to biggestTx).foldLeft((generators, itemSetMeta)) {
      case ((prods, itemSetSupport), itemSetSize) => mergeFrequentItemSets(prods, itemSetSupport, minSupport, itemSetSize)
    }
    ???
  }

  def frequentItems(transactions: Vector[Transaction], minSupport: Support): ItemSetMeta = {
    transactions.flatMap(tx => tx.items.map(_ -> tx.id)).foldLeft(Map.empty[Item, TransactionSet]) {
      case (occurrence, (item: Item, id: TransactionId)) if !occurrence.contains(item) =>
        occurrence + (item -> Set(id))
      case (occurrence, (item: Item, id: TransactionId)) if !occurrence(item).contains(id) =>
        occurrence + (item -> (occurrence(item) + id))
      case (occurrence, _) =>
        occurrence
    }.flatMap {
      case (item, txs) if txs.size >= minSupport =>
        val singletonItemSet = Set(item)
        Some(singletonItemSet -> ItemSetMetaData(singletonItemSet, Support(txs.size), Set.empty[Item]))
      case _ => None
    }
  }

  def mergeFrequentItemSets(generators: ItemSetGenerators, itemSetMeta: ItemSetMeta, minSupport: Support, itemSetSize: Int): (ItemSetGenerators, ItemSetMeta) = {
    ???
  }
}

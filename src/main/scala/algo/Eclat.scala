package com.mkobiers.med
package algo

import domain._

object Eclat extends AssociationRulesFinder {

  override def associationRules(
      transactions: Vector[Transaction],
      minSupport: MinSupport,
      minConfidence: MinConfidence
  ): Vector[Rule] = {
    val fItemSets = frequentItemSets(transactions, minSupport)
    fItemSets.keys.filter(_.nonEmpty).foldLeft(Vector.empty[Rule]) {
      (rlz, itemSet) =>
        rlz ++ rulesFromItemSet(itemSet, fItemSets, minConfidence, transactions)
    }
  }

  def rulesFromItemSet(
      itemSet: ItemSet,
      itemSetSupport: Map[ItemSet, Support],
      minConfidence: MinConfidence,
      transactions: Vector[Transaction]
  ): Vector[Rule] = {
    itemSet.subsets
      .map(predecessor => predecessor -> itemSet.diff(predecessor))
      .filter { case (predecessor, successor) =>
        predecessor.nonEmpty && successor.nonEmpty
      }
      .filter { case (predecessor, successor) =>
        itemSetSupport.contains(predecessor) && itemSetSupport.contains(
          successor
        )
      }
      .map { case (predecessor, successor) =>
        val confidence = Confidence(
          itemSetSupport(itemSet).txs.size / itemSetSupport(
            predecessor
          ).txs.size
        )
        val metrics = Metric.calculate(
          predecessor,
          successor,
          confidence,
          itemSetSupport,
          transactions
        )
        Rule(
          predecessor,
          successor,
          itemSetSupport(itemSet),
          confidence,
          metrics
        )
      }
      .filter(_.confidence.value > minConfidence.value)
      .toVector
  }

  override def frequentItemSets(
      transactions: Vector[Transaction],
      minSupport: MinSupport
  ): Map[ItemSet, Support] = {
    // operate on raw transaction set only once
    val itemSetMeta: ItemSetMeta      = frequentItems(transactions, minSupport)
    val generators: ItemSetGenerators = itemSetMeta.keys.toVector
    val biggestTx                     = transactions.map(_.items.size).max
    // merge all item sets with an assumption that maximal size of frequent item set can not exceed maximal tx size
    val _, support = (1 to biggestTx).foldLeft(
      (generators: ItemSetGenerators, itemSetMeta: ItemSetMeta)
    ) { case ((generators, meta), _) =>
      mergeFrequentItems(generators, meta, minSupport)
    }
    support._2.map { case (itemSet, meta) => itemSet -> meta.support }
  }

  def frequentItems(
      transactions: Vector[Transaction],
      minSupport: MinSupport
  ): ItemSetMeta = {
    val itemCount = transactions
      .flatMap(tx => tx.items.map(_ -> tx.id))
      .foldLeft(Map.empty[Item, TransactionSet]) {
        case (occurrence, (item: Item, id: TransactionId))
            if !occurrence.contains(item) =>
          occurrence + (item -> Set(id))
        case (occurrence, (item: Item, id: TransactionId))
            if !occurrence(item).contains(id) =>
          occurrence + (item -> (occurrence(item) + id))
        case (occurrence, _) =>
          occurrence
      }
    // create tree structure of item sets with empty item set as root and singleton item sets as first leaves
    val root = Set.empty[Item]
    val treeLikeMetaData = Map[ItemSet, ItemSetMetaData](
      root -> ItemSetMetaData(root, Support(transactions.map(_.id).toSet), root)
    )
    itemCount.foldLeft(treeLikeMetaData: ItemSetMeta) {
      case (meta, (item, txs)) if txs.size >= minSupport.value =>
        val singletonItemSet = Set(item)
        val singletonMetaData =
          ItemSetMetaData(singletonItemSet, Support(txs), root)
        // update tree structure - add new leaf as root successor
        meta + (root -> meta(root).copy(successors =
          meta(root).successors + singletonItemSet
        )) + (singletonItemSet -> singletonMetaData)
      case (meta, _) => meta
    }
  }

  def mergeFrequentItems(
      generators: ItemSetGenerators,
      itemSetMeta: ItemSetMeta,
      minSupport: MinSupport
  ): (ItemSetGenerators, ItemSetMeta) = {
    val itemSets: Set[(ItemSet, ItemSet)] =
      potentialItemSets(generators, itemSetMeta)

    itemSets.foldLeft(
      (Vector.empty[ItemSet]: ItemSetGenerators, itemSetMeta: ItemSetMeta)
    ) {
      case ((nextGenerators, meta), (left, right))
          if combinedItemSetSupport(
            meta,
            left,
            right
          ).txs.size >= minSupport.value =>
        val product: ItemSet = left ++ right
        val productSupport =
          Support(meta(left).support.txs intersect meta(right).support.txs)
        val productMeta = ItemSetMetaData(product, productSupport, left)
        val nextMeta = meta + (left -> meta(left).copy(successors =
          meta(left).successors + product
        )) + (product -> productMeta)
        (nextGenerators :+ product, nextMeta)
      case ((nextGenerators, meta), _) => (nextGenerators, meta)
    }
  }

  def potentialItemSets(
      generators: ItemSetGenerators,
      itemSetMeta: ItemSetMeta
  ): Set[(ItemSet, ItemSet)] = {
    val predecessors =
      generators.map(itemSet => itemSetMeta(itemSet).predecessor).toSet
    predecessors.foldLeft(Set.empty[(ItemSet, ItemSet)]) {
      case (acc, predecessor) =>
        val successorPairs = for {
          gen1 <- itemSetMeta(predecessor).successors
          gen2 <- itemSetMeta(predecessor).successors.filterNot(_ == gen1)
        } yield {
          // prevent pair duplicates
          if (gen1.hashCode() > gen2.hashCode()) gen1 -> gen2 else gen2 -> gen1
        }
        acc ++ successorPairs
    }
  }

  def combinedItemSetSupport(
      itemSetMeta: ItemSetMeta,
      left: ItemSet,
      right: ItemSet
  ): Support = {
    Support(
      itemSetMeta(left).support.txs intersect itemSetMeta(right).support.txs
    )
  }
}

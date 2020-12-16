package com.mkobiers.med
package algo

import domain._

import org.slf4j.{Logger, LoggerFactory}

object Eclat extends AssociationRulesFinder {

  case class PartialResult(
      generators: ItemSetGenerators,
      itemSetMeta: ItemSetMeta
  )

  val log: Logger = LoggerFactory.getLogger(Eclat.getClass)

  override def associationRules(
      transactions: Vector[Transaction],
      minSupport: MinSupport,
      minConfidence: MinConfidence
  ): Map[RuleId, Rule] = {
    log.info("searching for frequent item sets")
    val fItemSets = frequentItemSets(transactions, minSupport)
    log.info(s"${fItemSets.size} frequent item sets found")
    val transactionIds = transactions.map(_.id).toSet
    fItemSets.keys.filter(_.nonEmpty).foldLeft(Map.empty[RuleId, Rule]) {
      (rlz, itemSet) =>
        rlz ++ rulesFromItemSet(
          itemSet,
          id => !rlz.contains(id),
          fItemSets,
          minConfidence,
          transactionIds
        )
    }
  }

  def rulesFromItemSet(
      itemSet: ItemSet,
      ruleFilter: RuleId => Boolean,
      itemSetSupport: Map[ItemSet, Support],
      minConfidence: MinConfidence,
      transactionIds: Set[TransactionId]
  ): Map[RuleId, Rule] = {
    itemSet
      .subsets()
      .map(predecessor => RuleId(predecessor, itemSet.diff(predecessor)))
      .filter(ruleFilter)
      .filter(ruleId =>
        ruleId.predecessor.nonEmpty && ruleId.successor.nonEmpty
      )
      .filter(ruleId =>
        itemSetSupport.contains(ruleId.predecessor) && itemSetSupport.contains(
          ruleId.successor
        )
      )
      .map(ruleId => {
        val baseSup = BigDecimal(itemSetSupport(itemSet).txs.size)
        val predSup = BigDecimal(itemSetSupport(ruleId.predecessor).txs.size)
        ruleId -> Confidence(baseSup / predSup)
      })
      .filter { case (_, confidence) =>
        confidence.value >= minConfidence.value
      }
      .map { case (ruleId, confidence) =>
        val metrics = Metric.calculate(
          ruleId.predecessor,
          ruleId.successor,
          confidence,
          itemSetSupport,
          transactionIds
        )
        ruleId -> Rule(
          ruleId,
          itemSetSupport(itemSet),
          confidence,
          metrics
        )
      }
      .toMap
  }

  override def frequentItemSets(
      transactions: Vector[Transaction],
      minSupport: MinSupport
  ): Map[ItemSet, Support] = {
    log.info(s"checking item sets of length equal 1")
    // operate on raw transaction set only once
    val itemSetMeta: ItemSetMeta      = frequentItems(transactions, minSupport)
    val generators: ItemSetGenerators = itemSetMeta.keys.toVector
    val biggestTx: Int                = transactions.map(_.items.size).max
    // merge all item sets with an assumption that maximal size of frequent item set can not exceed maximal tx size
    val result =
      (2 to biggestTx).foldLeft(PartialResult(generators, itemSetMeta)) {
        case (PartialResult(gens, meta), length) =>
          log.info(s"checking item sets of length equal $length")
          mergeFrequentItems(gens, meta, minSupport)
      }
    result.itemSetMeta.map { case (itemSet, meta) => itemSet -> meta.support }
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
  ): PartialResult = {
    val itemSets: Set[(ItemSet, ItemSet)] =
      potentialItemSets(generators, itemSetMeta)

    itemSets.foldLeft(
      PartialResult(Vector.empty[ItemSet], itemSetMeta)
    ) {
      case (PartialResult(nextGens, meta), (left, right))
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
        PartialResult(nextGens :+ product, nextMeta)
      case (res: PartialResult, _) => res
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

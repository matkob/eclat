package com.mkobiers.med

package object domain {
  type TransactionSet    = Set[TransactionId]
  type ItemSet           = Set[Item]
  type ItemSetMeta       = Map[ItemSet, ItemSetMetaData]
  type ItemSetGenerators = Vector[ItemSet]
}

package com.mkobiers.med
package domain

case class ItemSetMetaData(
    itemSet: ItemSet,
    support: Support,
    predecessor: ItemSet,
    successors: Set[ItemSet] = Set.empty[ItemSet]
)

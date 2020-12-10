package com.mkobiers.med
package domain

case class Transaction(id: TransactionId, items: Vector[Item])
